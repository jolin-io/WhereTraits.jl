module Syntax
export @traits, @traits_test, @traits_show_implementation

using DataTypesBasic
using ASTParser
using SimpleMatch
using Traits: CONFIG
using Traits.Utils
include("astparser.jl")
include("Lowering.jl")
using .Lowering
include("NormalizeType.jl")
using .NormalizeType
include("Utils.jl")
using .Utils
using Suppressor
using ProxyInterface
using Markdown

"""
@traits f(a, b) where {!isempty(a), !isempty(b)} = (a[1], b[1])
"""
macro traits(expr_original)
  expr = macroexpand(__module__, expr_original)
  expr_traits = _traits(@MacroEnv, expr, expr_original)
  expr_traits = esc(expr_traits)
  if CONFIG.suppress_on_traits_definitions
    expr_traits = :(@suppress $expr_traits)
  end
  expr_traits
end


"""
like @traits, and works within Test.@testset, but cannot be doc-stringed

needed because of https://github.com/JuliaLang/julia/issues/34263
"""
macro traits_test(expr_original)
  expr = macroexpand(__module__, expr_original)
  expr_traits = _traits(@MacroEnv, expr, expr_original)
  # :(eval($(QuoteNode(...))) is a workaround for @testset, see https://github.com/JuliaLang/julia/issues/34263
  expr_traits = :(eval($(QuoteNode(expr_traits))))
  expr_traits = esc(expr_traits)
  if CONFIG.suppress_on_traits_definitions
    expr_traits = :(@suppress $expr_traits)
  end
  expr_traits
end


function _traits(env, expr::Expr, expr_original::Expr)
  parser = Matchers.AnyOf(Parsers.Function(), anything)
  _traits_parsed(env, parser(expr), expr_original)
end

function _traits_parsed(env, func_parsed::Parsers.Function_Parsed, expr_original::Expr)
  mod_traitsdef, store::TraitsStore = getorcreatestore!(env.mod, func_parsed.name)
  func_parsed_original = Parsers.Function()(expr_original)

  basefunc, lowerings = lower_args_default(func_parsed)
  basefunc_ori, lowerings_ori = lower_args_default(func_parsed_original)
  basefunc_outer, basefunc_inner = TraitsFunctionParsed(env, basefunc, toAST(basefunc_ori))
  store, base_update, base_torender = merge(store, basefunc_outer, basefunc_inner)
  exprs = Any[
    render_store_reference(env, store),
    render(env, store, base_torender)]
  if CONFIG.auto_documentation
    push!(exprs, render_doc(env, store, base_update))
  end

  for (f, f_ori) in zip(lowerings, lowerings_ori)
    # As lowering dropped variables, also traits may need to be dropped. Do this silently.
    lowered_outer, lowered_inner = TraitsFunctionParsed(env, f, toAST(f_ori), on_traits_dropped = msg -> nothing)
    store, lowered_update, lowered_torender = merge(store, lowered_outer, lowered_inner)
    push!(exprs, render(env, store, lowered_torender))
  end
  # finally return nothing in order to not return implementation detail
  flatten_blocks(Expr(:block, exprs..., nothing))
end


function _traits_parsed(env, parsed)
  throw(ArgumentError("@traits macro expects function expression"))
end

# # TODO should we Deprecate this syntax?
# function _traits(mod, block_parsed::Parsers.Block_Parsed)
#   # @traits on block doesn't use any global state
#   store = TraitsStore()
#   parser = Matchers.AnyOf(Parsers.Function(), anything)
#   funcs = [p for p in [parser(a) for a in block_parsed.exprs] if p isa Parsers.Function]
#   for f in funcs
#     outerfunc, innerfunc = TraitsFunctionParsed(mod, f)
#     merge!(store, outerfunc, innerfunc)
#   end
#   render(store)
# end

# Internal State of the syntax
# ============================

# this syntax is so complex that we need to store a state for each function

const InnerFunc = Any
const InnerFuncs = Dict{Any, Any}  # Dict{FixedPart, NonFixedPart}
const OuterFunc = Any
const DescriptionOfOneTraitsFunction = Tuple{OuterFunc, InnerFuncs}
const SignatureDict = TypeDict{DescriptionOfOneTraitsFunction}
struct Reference
  mod::Module
  isoriginalmodule::Bool
  name::Symbol
end
ASTParser.toAST(r::Reference) = :($(r.mod).$(r.name))

struct TraitsStore
  # we need to be able to overwrite not only the outer function, but also the inner function
  # hence we need a global reference for the inner function
  # as we cannot assume that the original function already used traits, we store the inner function
  # as part of every TraitsStore
  global_innerfunction_reference::Reference
  # maps a type signature to the respective outerfunction with possible several innerfunction-definitions
  definitions::SignatureDict
end
TraitsStore(global_innerfunction_reference::Reference) = TraitsStore(global_innerfunction_reference, SignatureDict())
ProxyInterface.dict(store::TraitsStore) = store.definitions
ProxyInterface.dict(Store::Type{TraitsStore}) = SignatureDict
ProxyInterface.@dict_mutable TraitsStore
function Base.copy(store::TraitsStore)
  TraitsStore(store.global_innerfunction_reference, copy(store.definitions))
end


# to get an easy fealing of what is going on and inspect errors
macro traits_show_implementation(funcname)
  traits_show_implementation(@MacroEnv, funcname)
end

function traits_show_implementation(env, funcname)
  mod_traitsdef, store = getorcreatestore!(env.mod, funcname)
  traits_show_implementation(env, store)
end

"""
render a whole TraitsStore

for debugging purposes only
"""
function traits_show_implementation(env::MacroEnv, store::TraitsStore)
  exprs = []
  for (outerfunc, innerfuncs) in values(store)
    push!(exprs, Markdown.parse("Outer function for signature $(outerfunc.fixed.signature)"))
    push!(exprs, Markdown.parse("""
    ```
    $(render(env, store, RenderOuterFunc(outerfunc))))
    ```
    """))
    push!(exprs, Markdown.parse("\n- - -\n"))
    push!(exprs, Markdown.parse("Inner functions for signature $(outerfunc.fixed.signature)"))
    for (fixed, nonfixed) in innerfuncs
      innerfunc = (fixed = fixed, nonfixed = nonfixed)
      push!(exprs, Markdown.parse("""
      ```
      $(render(env, store, RenderInnerFunc(outerfunc, innerfunc)))
      ```
      """))
      push!(exprs, Markdown.parse("\n- - -\n"))
    end
    # nice separtor between several outer functions
    push!(exprs, Markdown.HorizontalRule())
  end
  Markdown.MD(exprs...)
end


# implementation
# ##############

# here the reference to the internal state for the @traits macro is stored for each functionname respectively
# if the function has no key, then it is defined in the original module
const traits_store = Dict{Symbol, Reference}()

function getorcreatestore!(mod, funcname)
  mod_original, _ = normalize_mod_and_name(mod, funcname)
  inner_function_name = unique_inner_function_name(mod, funcname)
  if isdefined(mod_original, inner_function_name)
    # call with no args to get the store
    mod_original, getproperty(mod_original, inner_function_name)()
  elseif haskey(traits_store, inner_function_name)
    ref = traits_store[inner_function_name]
    # call with no args to get the store
    ref.mod, getproperty(ref.mod, ref.name)()
  else
    # if nothing is defined yet default to current module `mod` for definition
    # as we can define new things only in the current module
    isoriginalmodule = mod === mod_original
    ref = Reference(mod, isoriginalmodule, inner_function_name)
    if !isoriginalmodule
      # if we are not in the original module we need to store the reference for others to find the traits definitions
      # we are going to safe the TraitsStore on the function ``ref`` with 0 arguments
      traits_store[inner_function_name] = ref
    end
    mod, TraitsStore(ref)
  end
end

abstract type RenderType end
struct RenderOuterFunc{O} <: RenderType
  outer::O
end
struct RenderOuterAndInnerFuncs{O, Is} <: RenderType
  outer::O
  inners::Is
end
struct RenderInnerFunc{O, I} <: RenderType
  # we need the outerfunc to construct the innerfunc, as it depends on the ordering of the traits functions
  # which are defined in the outerfunc
  outer::O
  inner::I
end

struct TraitsUpdate{O, Is, I}
  outer::O
  inners::Is
  inner::I
end


"""
merge the new traits information into the given traitsstore
and return whatever needs to be rendered for a correct update of the traits
"""
function Base.merge(store::TraitsStore, outerfunc, innerfunc)
  signature = outerfunc.fixed.signature
  # update TraitsStore and return what to render
  store_new = copy(store)
  torender = if haskey(store, signature)
    outerfunc_old, innerfuncs = store[signature]

    innerfuncs_new = copy(innerfuncs)
    innerfuncs_new[innerfunc.fixed] = innerfunc.nonfixed

    outerfunc_new_nonfixed = (
      # we aggregate all unique traits and ensure order
      innerargs_traits = sortexpr(unique([outerfunc_old.nonfixed.innerargs_traits; outerfunc.nonfixed.innerargs_traits])),
    )

    if outerfunc_old.nonfixed == outerfunc_new_nonfixed  # if same Traits, only the inner function needs to be rendered
      store_new[signature] = (outerfunc_old, innerfuncs_new)
      RenderInnerFunc(outerfunc_old, innerfunc)
    else
      outerfunc_new = (
        # outerfunc.fixed == outerfunc_old.fixed, because of same signature
        fixed = outerfunc_old.fixed,
        nonfixed = outerfunc_new_nonfixed,
      )
      store_new[signature] = (outerfunc_new, innerfuncs_new)
      RenderOuterAndInnerFuncs(outerfunc_new, innerfuncs_new)
    end
  else
    # initial case
    innerfuncs_new = InnerFuncs()
    innerfuncs_new[innerfunc.fixed] = innerfunc.nonfixed
    store_new[signature] = (outerfunc, innerfuncs_new)
    RenderOuterAndInnerFuncs(outerfunc, innerfuncs_new)
  end
  # also return all the information about the state after this merge within an update variable
  (outerfunc, innerfuncs) = store_new[signature]
  update = TraitsUpdate(outerfunc, innerfuncs, innerfunc)
  store_new, update, torender
end


# Render
# ======

# we use special Singletons as separators to distinguish different kinds of parameters
struct _BetweenTypeVarsAndTraits end
struct _BetweenArgsAndTypeVars end

function render_store_reference(env::MacroEnv, store::TraitsStore)
  name = store.global_innerfunction_reference
  if env.mod === name.mod
    # if we are rendering code for the same module, we need to drop the module information
    # this is needed for defining the function the very first time as ``MyModule.func(...) = ...`` is invalid syntax
    # for the initial definition
    name = name.name
  end
  :(function $(toAST(name))()
    $store
  end)
end

"""
render a whole TraitsStore

for debugging purposes only
"""
function render(env::MacroEnv, store::TraitsStore)
  exprs = []
  for (outerfunc, innerfuncs) in values(store)
    push!(exprs, render(env, store, RenderOuterFunc(outerfunc)))
    for (fixed, nonfixed) in innerfuncs
      innerfunc = (fixed = fixed, nonfixed = nonfixed)
      push!(exprs, render(env, store, RenderInnerFunc(outerfunc, innerfunc)))
    end
  end
  flatten_blocks(Expr(:block, exprs...))
end

"""
rerender one single outerfunc and respective innerfuncs
"""
function render(env::MacroEnv, store::TraitsStore, torender::RenderOuterAndInnerFuncs)
  outerfunc, innerfuncs = torender.outer, torender.inners
  exprs = []
  push!(exprs, render(env, store, RenderOuterFunc(outerfunc)))
  for (fixed, nonfixed) in innerfuncs
    innerfunc = (fixed = fixed, nonfixed = nonfixed)
    push!(exprs, render(env, store, RenderInnerFunc(outerfunc, innerfunc)))
  end
  flatten_blocks(Expr(:block, exprs...))
end

function _map_args(new_to_old, innerargs)
  map(innerargs) do a
    # pure ``_`` is currently buggy, see https://github.com/JuliaLang/julia/issues/32727
    # hence we use ::Any instead
    get(new_to_old, a, Expr(:(::), Symbol("'", a, "'"), :(Any)))
  end
end

"""
render innerfunction
(this is only possible with informations from outerfunc)
"""
function render(env::MacroEnv, store::TraitsStore, torender::RenderInnerFunc)
  outerfunc, innerfunc = torender.outer, torender.inner
  args = [
    # we need to dispatch on the signature so that different outerfuncs don't
    # overwrite each other's innerfunc
    :(::$(Type{outerfunc.fixed.signature}));
    _map_args(innerfunc.fixed.args_mapping, outerfunc.fixed.innerargs_args);
    :(::$_BetweenArgsAndTypeVars);
    _map_args(innerfunc.fixed.typevars_mapping, outerfunc.fixed.innerargs_typevars);
    :(::$_BetweenTypeVarsAndTraits);
    _map_args(innerfunc.fixed.traits_mapping, outerfunc.nonfixed.innerargs_traits);
  ]
  # if we are rendering code for the same module, we need to drop the module information
  # this is needed for defining the function the very first time as ``MyModule.func(...) = ...`` is invalid syntax
  # for the initial definition
  name = store.global_innerfunction_reference
  if env.mod === name.mod
    name = name.name
  end

  innerfunc_parsed = Parsers.Function_Parsed(
    name = name,
    curlies = [],
    args = args,
    kwargs = innerfunc.nonfixed.kwargs,
    wheres = [],
    body = innerfunc.nonfixed.body
  )
  toAST(innerfunc_parsed)
end

"""
render outer function
"""
function render(env::MacroEnv, store::TraitsStore, torender::RenderOuterFunc)
  outerfunc = torender.outer

  innerargs = [
    # we need to dispatch on the signature so that different outerfuncs don't
    # overwrite each other's innerfunc
    outerfunc.fixed.signature;
    outerfunc.fixed.innerargs_args;
    _BetweenArgsAndTypeVars();
    outerfunc.fixed.innerargs_typevars;
    _BetweenTypeVarsAndTraits();
    outerfunc.nonfixed.innerargs_traits;
  ]
  innerfunc_call = Parsers.Call_Parsed(
    name = store.global_innerfunction_reference,
    curlies = [],
    args = innerargs,
    kwargs = [:(kwargs...)],
  )
  # add LineNumberNode for debugging purposes
  body = Expr(:block, env.source, innerfunc_call)

  outerfunc_parsed = Parsers.Function_Parsed(
    name = outerfunc.fixed.name,
    curlies = outerfunc.fixed.curlies,
    args = outerfunc.fixed.args,
    kwargs = [:(kwargs...)],
    wheres = outerfunc.fixed.wheres,
    body = body,
  )
  toAST(outerfunc_parsed)
end

"""
render documentation

extra effort needs to be done to properly document the outer function by referring
to innerfunctions
"""
function render_doc(env::MacroEnv, store::TraitsStore, torender::TraitsUpdate)
  outerfunc = torender.outer
  innerfuncs = torender.inners
  innerfunc = torender.inner

  signature = toAST(Parsers.Signature_Parsed(
    name = outerfunc.fixed.name,
    curlies = outerfunc.fixed.curlies,
    args = outerfunc.fixed.args,
    kwargs = [:(kwargs...)],
    wheres = outerfunc.fixed.wheres,
  ))

  # start documentation with autosignature of outer function
  header = Markdown.parse("""
  ```
  $signature
  ```
  ------ Original @traits definitions follow ------

  """)
  separator = Markdown.parse("- - -\n")

  doc_exprs = Any[header]
  for (fixed, nonfixed) in innerfuncs
    # automatic signature string of inner function
    signature_original = Markdown.parse("```julia\n$(nonfixed.expr_original.args[1])\n```")  # TODO this assumes that expr_original is a function, can we do this?
    push!(doc_exprs, signature_original)
    # manual doc string of respective inner function
    push!(doc_exprs, :(Traits.Utils.DocsHelper.mygetdoc(
      $(toAST(store.global_innerfunction_reference)),
      Tuple{Type{$(outerfunc.fixed.signature)}, Type{$(innerfunc_fixed_to_doctype(fixed))}})))
    # automatic doc string of inner function definition
    expr_original = Markdown.parse("Original @traits definition:\n```julia\n$(nonfixed.expr_original)\n```")
    push!(doc_exprs, expr_original)
    # better visual separation
    push!(doc_exprs, separator)
  end
  # get rid of last separator
  deleteat!(doc_exprs, lastindex(doc_exprs))

  # if we are rendering code for the same module, we need to drop the module information
  # this is needed for defining the function the very first time as ``MyModule.func(...) = ...`` is invalid syntax
  # for the initial definition
  name = store.global_innerfunction_reference
  if env.mod === name.mod
    name = name.name
  end
  name = toAST(name)

  quote
    # first the documentation of the inner function as this needs to be updated BEFORE the outer doc-string
    # is updated below
    Base.@__doc__ function $name(::Type{$(outerfunc.fixed.signature)}, ::Type{$(innerfunc_fixed_to_doctype(innerfunc.fixed))}) end

    # documentation of outer function (we need to manually ignore nothing docs)
    let docstring = Base.Docs.catdoc(filter(!isnothing, [$(doc_exprs...)])...)
      Traits.Utils.DocsHelper.@doc_signature docstring ($signature)
    end
  end
end


struct InnerFuncFixedDocSig{Args, TypeVars, Traits} end
function innerfunc_fixed_to_doctype(innerfunc_fixed)
  dicts = [innerfunc_fixed.args_mapping, innerfunc_fixed.typevars_mapping, innerfunc_fixed.traits_mapping]
  types = Dict_to_normalizedType.(dicts)
  InnerFuncFixedDocSig{types...}
end

# Syntax Parser
# =============

TraitsFunctionParsed(env, expr::Expr, expr_original; kwargs...) = TraitsFunctionParsed(env, Parsers.Function()(expr), expr_original; kwargs...)
function TraitsFunctionParsed(env, func_parsed::Parsers.Function_Parsed, expr_original; on_traits_dropped = msg -> throw(ArgumentError(msg)))
  # Parse main syntax
  # =================
  args_names = [Parsers.Arg()(arg).name for arg in func_parsed.args]
  args_names_matcher = Matchers.AnyOf(args_names...)
  # TODO we could also allow dispatch on keyword arguments
  # kwargs_names = [Parsers.Arg()(kwarg).name for kwarg in func_parsed.kwargs]
  # kwargs_names_matcher = Matchers.AnyOf(kwargs_names...)

  whereexpr_parser = Matchers.AnyOf(
    # we allow dispatch on arguments
    Named{:arg}(args_names_matcher), # interpreted as bool
    Named{:arg}(Parsers.TypeAnnotation(name=args_names_matcher)),
    Named{:arg}(Parsers.TypeRange(name=args_names_matcher)),

    # TODO we could also allow dispatch on keyword arguments
    # How? we store all kwargs per signature and then define traitfunctions like ``func(kwargs[:b])``
    # Named{:kwarg}(kwargs_names_matcher), # interpreted as bool
    # Named{:kwarg}(Parsers.TypeAnnotation(name=kwargs_names_matcher)),
    # Named{:kwarg}(Parsers.TypeRange(name=kwargs_names_matcher)),

    # all other symbols refer to standard TypeVariables
    # TypeAnnotation on TypeVar make only sense in the special case that the typevariable can take different types of binary type
    # both Symbol and TypeRange are normal where syntax, TypeAnnotation is not
    Named{:normal}(Parsers.Symbol()),
    Named{:typevar}(Parsers.TypeAnnotation(name=Parsers.Symbol())),
    Named{:normal}(Parsers.TypeRange(name=Parsers.Symbol())),

    # we further allow to dispatch on functions from either args, kwargs, or standard typevars
    # which should be all the rest
    Named{:func}(Parsers.Call()),  # interpreted as bool
    Named{:func}(Parsers.TypeAnnotation()),
    Named{:func}(Parsers.TypeRange()),
  )
  parsed_wheres = map(whereexpr_parser, func_parsed.wheres)
  normal_wheres, extra_wheres = filtersplit(parsed_wheres) do x
    x isa Parsers.Named_Parsed{:normal}
  end


  # prepare extra wheres
  # ====================

  traits = map(extra_wheres) do expr
    @match(expr) do f
      f(x::Named_Parsed{:arg, <:Matchers.AnyOf}) = :(Val{$(toAST(x.expr))}())  # plain arguments are interpreted as bool
      # plain calls are assumed to refer to boolean expressions
      function f(x::Named_Parsed{:func, Parsers.Call})
        if x.expr.name == :!
          # if the last call is negation "!" we take the thing which is negated
          # as a trait function and dispatch on False
          :(Val{$(toAST(x.expr.args[1]))}())
        else
          :(Val{$(toAST(x.expr))}())
        end
      end
      f(x::Named_Parsed{<:Any, Parsers.TypeAnnotation}) = toAST(x.expr.name)
      f(x::Named_Parsed{<:Any, Parsers.TypeRange}) = toAST(x.expr.name)
    end
  end

  traits_matching_types = map(enumerate(extra_wheres)) do (i, expr)
    @match(expr) do f
      f(x::Named_Parsed{:arg, <:Matchers.AnyOf}) = :(Val{true})  # plain arguments are interpreted as bool
      f(x::Named_Parsed{:func, Parsers.Call}) = (x.expr.name == :!) ? :(Val{false}) : :(Val{true})  # plain calls are assumed to refer to boolean expressions
      f(x::Named_Parsed{<:Any, Parsers.TypeAnnotation}) = toAST(x.expr.type)
      function f(x::Named_Parsed{<:Any, Parsers.TypeRange})
        tr = x.expr
        @assert !(tr.lb === Union{} && tr.ub == Any) "should have at least an upperbound or a lowerbound"
        if tr.lb === Union{}  # only upperbound
          :(Type{<:$(toAST(tr.ub))})
        elseif tr.ub === Any  # only LowerBound
          :(Type{>:$(toAST(tr.lb))})
        else  # both
          :(Type{T} where {$(toAST(tr.lb)) <: T <: $(toAST(tr.up))})
        end
      end
    end
  end

  # normalize first function dispatch level
  # =======================================

  outerfunc_parsed = Parsers.Function_Parsed(
    name = func_parsed.name,
    curlies = func_parsed.curlies,
    args = func_parsed.args,
    kwargs = [],
    # we need to use toAST here, as normalize_func expects a
    # standard Function_Parsed, where wheres is a list of Expr
    wheres = toAST(normal_wheres),
    body = :nothing
  )

  outerfunc_fixed, innerfunc_fixed = normalize_func(env.mod, outerfunc_parsed)

  @assert(
    isempty(intersect(keys(innerfunc_fixed.args_mapping), keys(innerfunc_fixed.typevars_mapping))),
    "args and typevariables must not overlap!
    Found $(intersect(keys(innerfunc_fixed.args_mapping), keys(innerfunc_fixed.typevars_mapping))) in both.")

  typevars_old = map(normal_wheres) do parsed
    @match(parsed) do f
      f(x::Parsers.Named_Parsed{:normal, Parsers.Symbol}) = x.expr.symbol
      f(x::Parsers.Named_Parsed{:normal, Parsers.TypeRange}) = x.expr.name.symbol
    end
  end
  kept_typevars_old = values(innerfunc_fixed.typevars_mapping)
  dropped_typevars_old = [v for v in typevars_old if v ∉ kept_typevars_old]

  traits_filtered = filter(t -> !depends_on(t, dropped_typevars_old), traits)
  if length(traits) != length(traits_filtered)
    traits_dropped = setdiff(traits, traits_filtered)
    on_traits_dropped("Given traits depend on droppable typeparameters ($dropped_typevars_old). Traits: $traits_dropped")
  end

  old_to_new = Dict(v => k for (k, v) in merge(innerfunc_fixed.args_mapping, innerfunc_fixed.typevars_mapping))
  # TODO we currently do not normalize the traits function names
  # TODO e.g. using both ``Base.IteratorSize(a)`` and ``IteratorSize(a)`` result in two different traits currently
  traits_normalized = _change_symbols(traits_filtered, old_to_new)
  # we map the traitsdefinition to a signature including a name for easier debugging and the correct type
  traits_mapping = Dict(k => Expr(:(::), Symbol("'", k, "'"), v)
                        for (k, v) in zip(traits_normalized, traits_matching_types))
  # we may encounter no traits at all, namely in the case where a default clause is defined
  innerargs_traits = isempty(traits_normalized) ? [] : sortexpr(unique(traits_normalized))
  outerfunc′ = (
    fixed = outerfunc_fixed,
    nonfixed = (
      innerargs_traits = innerargs_traits,
    ),
  )
  innerfunc′ = (
    fixed = (
      args_mapping = innerfunc_fixed.args_mapping,
      typevars_mapping = innerfunc_fixed.typevars_mapping,
      traits_mapping = traits_mapping,
    ),
    nonfixed = (
      kwargs = func_parsed.kwargs,
      body = func_parsed.body,
      expr_original = expr_original,
    ),
  )
  outerfunc′, innerfunc′
end

_change_symbols(vec::Vector, symbol_mapping) = [_change_symbols(x, symbol_mapping) for x in vec]
_change_symbols(sym::Symbol, symbol_mapping) = get(symbol_mapping, sym, sym)
_change_symbols(any, symbol_mapping) = any
_change_symbols(qn::QuoteNode, symbol_mapping) = QuoteNode(_change_symbols(qn.value, symbol_mapping))
_change_symbols(expr::Expr, symbol_mapping) = Expr(expr.head, _change_symbols(expr.args, symbol_mapping)...)


# As part of the parsing, we need to normalization the outer function
# -------------------------------------------------------------------

struct _BetweenCurliesAndArgs end

normalize_func(mod, func_expr::Expr) = normalize_func(mod, Parsers.Function()(func_expr))
function normalize_func(mod, func_parsed::Parsers.Function_Parsed)
  args_parsed = map(Parsers.Arg(), func_parsed.args)
  @assert all(arg -> arg.default isa NoDefault, args_parsed) "Can only normalize functions without positional default arguments"

  # normalize types
  # ---------------
  # we add _BetweenCurliesAndArgs to reuse ``type`` as identifying signature without mixing curly types and arg types
  typeexpr = Expr(:curly, Tuple, func_parsed.curlies...,
    _BetweenCurliesAndArgs, (toAST(a.type) for a in args_parsed)...)
  typeexpr_full = :($typeexpr where {$(toAST(func_parsed.wheres)...)})
  type = Base.eval(mod, typeexpr_full)
  typebase, typevars_old = split_typevar(type)  # typebase == Tuple{P1, P2, P3, ...}
  typeexprs_new, typevars_new, typevar_new_to_old = normalize_typevars(typebase.parameters, typevars_old)

  curlies_typeexprs_new = typeexprs_new[1:length(func_parsed.curlies)]
  args_typeexprs_new = typeexprs_new[length(func_parsed.curlies)+2:end]  # + 2 because of _BetweenCurliesAndArgs

  # normalize args
  # --------------
  arg_new_to_old = Dict{Symbol, Symbol}()
  countfrom_args = Base.Iterators.Stateful(Base.Iterators.countfrom(1))
  for (a, type) in zip(args_parsed, args_typeexprs_new)
    a.type = type
    arg_position = next!(countfrom_args)
    new = normalized_arg_by_position(arg_position)
    if !isnothing(a.name)  # might be an arg without name
      arg_new_to_old[new] = a.name
    end
    a.name = new
  end

  # return
  # ------
  # we return the whole signature to easily decide whether the function overwrites another
  outerfunc_fixed = (
    # everything under fixed should be identifiable via signature
    signature = type,
    name = func_parsed.name,
    curlies = curlies_typeexprs_new,
    args = args_parsed,
    wheres = typevars_new,
    # body information
    innerargs_args = sort([a.name for a in args_parsed]),
    innerargs_typevars = sort([tv.name for tv in typevars_new]),
  )
  innerfunc_fixed = (
    args_mapping = arg_new_to_old,
    typevars_mapping = typevar_new_to_old,
  )
  outerfunc_fixed, innerfunc_fixed
end

normalized_arg_by_position(position::Int) = Symbol("a", position)

end # module
