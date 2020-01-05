module Syntax
export @traits, @traits_show_implementation, @traits_delete!

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
using Suppressor
using ProxyInterface

# flatten out blocks
flatten_blocks(expr::Expr) = flatten_blocks(Val{expr.head}(), expr.args)
flatten_blocks(any) = any
flatten_blocks(::Val{head}, args) where head = Expr(head, flatten_blocks.(args)...)
function flatten_blocks(head::Val{:block}, args)
  args′ = flatten_blocks.(args)
  args′′ = [((a isa Expr && a.head == :block) ? a.args : [a] for a in args′)...;]
  Expr(:block, args′′...)
end

"""
@traits f(a, b) where {!isempty(a), !isempty(b)} = (a[1], b[1])
"""
macro traits(expr)
  expr′ = macroexpand(__module__, expr)
  expr_traits = _traits(__module__, expr′)
  # :(eval($(QuoteNode(...))) is a workaround for @testset, see https://github.com/JuliaLang/julia/issues/34263
  expr_traits = :(eval($(QuoteNode(expr_traits))))
  expr_traits = esc(expr_traits)
  if CONFIG.suppress_on_traits_definitions
    expr_traits = :(@suppress $expr_traits)
  end
  expr_traits
end

function _traits(mod, expr::Expr)
  parser = Matchers.AnyOf(Parsers.Function(), anything)
  _traits_parsed(mod, parser(expr))
end

function _traits_parsed(mod, func_parsed::Parsers.Function_Parsed)
  store = getorcreatestore!(mod, func_parsed.name)

  basefunc, lowerings = lower_args_default(func_parsed)
  basefunc_outer, basefunc_inner = TraitsFunctionParsed(mod, basefunc)
  basefunc_new = merge!(store, basefunc_outer, basefunc_inner)

  exprs = Expr[render(mod, store, basefunc_new)]

  for f in lowerings
    # As lowering dropped variables, also traits may need to be dropped. Do this silently.
    lowered_outer, lowered_inner = TraitsFunctionParsed(mod, f, on_traits_dropped = msg -> nothing)
    lowered_new = merge!(store, lowered_outer, lowered_inner)
    push!(exprs, render(mod, store, lowered_new))
  end
  # finally return nothing in order to not return implementation detail
  flatten_blocks(Expr(:block, exprs..., nothing))
end


function _traits_parsed(mod, parsed)
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

# to get an easy fealing of what is going on and inspect errors
macro traits_show_implementation(funcname)
  QuoteNode(traits_show_implementation(__module__, funcname))
end

function traits_show_implementation(mod, funcname)
  mod, funcname = _true_mod_and_funcname(mod, funcname)
  render(mod, getorcreatestore!(mod, funcname))
end

function _true_mod_and_funcname(mod, funcname)
  parser = Matchers.AnyOf(Parsers.Symbol(), Parsers.NestedDot())
  @match(parser(funcname)) do f
    f(::Parsers.Symbol_Parsed) = mod, funcname
    function f(x::Parsers.NestedDot_Parsed)
      mod = getproperty(mod, x.base)
      for property in x.properties[1:end-1]
        mod = getproperty(mod, property)
      end
      mod, x.properties[end]
    end
  end
end

"""
  if you need to reset the traits definition of a certain function

Needed if you had a wrong reference in the traits functions
  which will always throw errors

CAUTION: this DOES NOT delete plain function definitions, but only the extra traits information
"""
macro traits_delete!(funcname)
  traits_delete!(__module__, funcname)
  QuoteNode(nothing)
end
function traits_delete!(mod, funcname)
  key = unique_funcname(mod, funcname)
  delete!(traits_store, key)
end

# Internal State of the syntax
# ============================

# this syntax is so complex that we need to store a state for each function

const InnerFunc = Any
const InnerFuncs = Dict{Any, Any}
const OuterFunc = Any
const SignatureDict = TypeDict{Tuple{OuterFunc, InnerFuncs}}
struct Reference
  mod::Module
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
  TraitsStore(global_innerfunction_reference::Reference) = new(global_innerfunction_reference, SignatureDict())
end
ProxyInterface.dict(store::TraitsStore) = store.definitions
ProxyInterface.dict(Store::Type{TraitsStore}) = SignatureDict
ProxyInterface.@dict_mutable TraitsStore

# here the internal state for the @traits macro is stored for each functionname respectively
const traits_store = Dict{Symbol, TraitsStore}()

function unique_funcname(mod, funcname)
  mod′, funcname′ = normalize_mod_and_name(mod, funcname)
  Symbol(mod′, :., funcname′)
end
function normalize_mod_and_name(mod, name)
  parser = Matchers.AnyOf(Parsers.Symbol(), Parsers.NestedDot())
  normalize_mod_and_name(mod, parser(name))
end
normalize_mod_and_name(mod, name::Parsers.Symbol_Parsed) = mod, name.symbol
function normalize_mod_and_name(mod, name::Parsers.NestedDot_Parsed)
  mod′::Module = getproperty(mod, name.base)
  for field in name.properties[1:end-1]
    mod′ = getproperty(mod′, field)
  end
  mod′, name.properties[end]  # NestedDot.properties is known to be non-empty
end

function getorcreatestore!(mod, funcname)
  key = unique_funcname(mod, funcname)
  if haskey(traits_store, key)
    traits_store[key]
  else
    inner_function_name = Symbol("'", "__traits__.", key, "'")
    global_inner_function_reference = Reference(mod, inner_function_name)
    newstore = TraitsStore(global_inner_function_reference)
    traits_store[key] = newstore
    newstore
  end
end

function merge!(store, outerfunc, innerfunc)
  signature = outerfunc.fixed.signature
  if haskey(store, signature)
    outerfunc_old, innerfuncs = store[signature]
    innerfuncs[innerfunc.fixed] = innerfunc.nonfixed

    outerfunc_new_nonfixed = (
      # we aggregate all unique traits and ensure order
      innerargs_traits = sortexpr(unique([outerfunc_old.nonfixed.innerargs_traits; outerfunc.nonfixed.innerargs_traits])),
    )

    if  outerfunc_old.nonfixed == outerfunc_new_nonfixed
      # only the new innerfunc is new
      # still we need outerfunc' for the information which traits are currently used in total
      outerfunc_old, innerfunc
    else
      outerfunc_new = (
        # outerfunc.fixed == outerfunc_old.fixed, because of same signature
        fixed = outerfunc_old.fixed,
        nonfixed = outerfunc_new_nonfixed,
      )
      update = (outerfunc_new, innerfuncs)
      store[signature] = update
      update
    end
  else
    innerfuncs = InnerFuncs()
    innerfuncs[innerfunc.fixed] = innerfunc.nonfixed
    initial = (outerfunc, innerfuncs)
    store[signature] = initial
    initial
  end
end


# Render
# ======

# we use special Singletons as separators to distinguish different kinds of parameters
struct _BetweenTypeVarsAndTraits end
struct _BetweenArgsAndTypeVars end

# render needs the module where it should render, because the syntax ``MyModule.B(args) = ...`` does only work for
# method overloading (where it is the only syntax), but not for DEFINING the same function initially. (For that you
# need to be within MyModule and execute ``B(args) = ...``)
function render(mod::Module, store::TraitsStore)
  exprs = []
  for (outerfunc, innerfuncs) in values(store)
    push!(exprs, render(mod, store, outerfunc))
    for (fixed, nonfixed) in innerfuncs
      innerfunc = (fixed = fixed, nonfixed = nonfixed)
      push!(exprs, render(mod, store, outerfunc, innerfunc))
    end
  end
  flatten_blocks(Expr(:block, exprs...))
end

function render(mod::Module, store::TraitsStore, outerfunc_innerfuncs::Tuple{OuterFunc, InnerFuncs})
  outerfunc, innerfuncs = outerfunc_innerfuncs
  exprs = []
  push!(exprs, render(mod, store, outerfunc))
  for (fixed, nonfixed) in innerfuncs
    innerfunc = (fixed = fixed, nonfixed = nonfixed)
    push!(exprs, render(mod, store, outerfunc, innerfunc))
  end
  flatten_blocks(Expr(:block, exprs...))
end

function render(mod::Module, store::TraitsStore, outerfunc_innerfunc::Tuple{OuterFunc, InnerFunc})
  outerfunc, innerfunc = outerfunc_innerfunc
  render(mod, store, outerfunc, innerfunc)
end

function _map_args(new_to_old, innerargs)
  map(innerargs) do a
    # pure ``_`` is currently buggy, see https://github.com/JuliaLang/julia/issues/32727
    # hence we use ::Any instead
    get(new_to_old, a, :(::Any))
  end
end

"""
render innerfunction
(this is only possible with informations from outerfunc)
"""
function render(mod::Module, store::TraitsStore, outerfunc, innerfunc)
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
  name = store.global_innerfunction_reference
  if mod === name.mod
    # if we are rendering code for the same module, we need to drop the module information
    # this is needed for defining the function the very first time as ``MyModule.func(...) = ...`` is invalid syntax
    # for the initial definition
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

function render(mod::Module, store::TraitsStore, outerfunc)
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

  outerfunc_parsed = Parsers.Function_Parsed(
    name = outerfunc.fixed.name,
    curlies = outerfunc.fixed.curlies,
    args = outerfunc.fixed.args,
    kwargs = [:(kwargs...)],
    wheres = outerfunc.fixed.wheres,
    body = innerfunc_call
  )
  # the outer function should become documented
  :(Base.@__doc__ $(toAST(outerfunc_parsed)))
end



# Syntax Parser
# =============

TraitsFunctionParsed(mod, expr::Expr; kwargs...) = TraitsFunctionParsed(mod, Parsers.Function()(expr); kwargs...)
function TraitsFunctionParsed(mod, func_parsed::Parsers.Function_Parsed; on_traits_dropped = msg -> throw(ArgumentError(msg)))
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

  traits_matches = map(enumerate(extra_wheres)) do (i, expr)
    @match(expr) do f
      f(x::Named_Parsed{:arg, <:Matchers.AnyOf}) = :(::Val{true})  # plain arguments are interpreted as bool
      f(x::Named_Parsed{:func, Parsers.Call}) = x.expr.name == :! ? :(::Val{false}) : :(::Val{true}) # plain calls are assumed to refer to boolean expressions
      f(x::Named_Parsed{<:Any, Parsers.TypeAnnotation}) = Expr(:(::), toAST(x.expr.type))
      function f(x::Named_Parsed{<:Any, Parsers.TypeRange})
        tr = x.expr
        @assert !(tr.lb === Union{} && tr.ub == Any) "should have at least an upperbound or a lowerbound"
        if tr.lb === Union{}  # only upperbound
          :(::Type{<:$(toAST(tr.ub))})
        elseif tr.ub === Any  # only LowerBound
          :(::Type{>:$(toAST(tr.lb))})
        else  # both
          :(::Type{T} where {$(toAST(tr.lb)) <: T <: $(toAST(tr.up))})
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

  outerfunc_fixed, innerfunc_fixed = normalize_func(mod, outerfunc_parsed)

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
  traits_normalized = _change_symbols(traits_filtered, old_to_new)
  traits_mapping = Dict(zip(traits_normalized, traits_matches))
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
    ),
  )
  outerfunc′, innerfunc′
end

_change_symbols(vec::Vector, symbol_mapping) = [_change_symbols(x, symbol_mapping) for x in vec]
_change_symbols(sym::Symbol, symbol_mapping) = get(symbol_mapping, sym, sym)
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
