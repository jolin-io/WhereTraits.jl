module Syntax
export @traits, @traits_show_implementation, @traits_delete!

using DataTypesBasic
using ASTParser
using SimpleMatch
using Traits.Utils
include("astparser.jl")
include("Lowering.jl")
using .Lowering



macro traits(expr)
  esc(_traits(__module__, expr))
end

function _traits(mod, expr::Expr)
  parser = Matchers.AnyOf(Parsers.Function(), anything)
  _traits(mod, parser(expr))
end

function _traits(mod, func_parsed::Parsers.Function_Parsed)
  store = getorcreatestore!(mod, func_parsed.name)

  basefunc, lowerings = lower_args_default(func_parsed)
  basefunc_outer, basefunc_inner = TraitsFunctionParsed(mod, basefunc)
  basefunc_new = merge!(store, basefunc_outer, basefunc_inner)

  exprs = Expr[render(basefunc_new)]

  for f in lowerings
    # Lowerings have only dependencies on args get dropped,
    # Dependencies on respective typevars still need to be dropped.
    # Do this silently.
    outerfunc, innerfunc = TraitsFunctionParsed(mod, f, on_traits_dropped = msg -> nothing)
    newfunc = merge!(store, outerfunc, innerfunc)
    push!(exprs, render(newfunc))
  end
  # finally return nothing in order to not return implementation detail
  Expr(:block, exprs..., nothing)
end


function _traits(mod, parsed)
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
  render(getorcreatestore!(mod, funcname))
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
  mod, funcname = _true_mod_and_funcname(mod, funcname)
  key = (Symbol(string(mod)), funcname)
  delete!(traits_store, key)
end

# Internal State of the syntax
# ============================

# this syntax is so complex that we need to store a state for each function

const InnerFunc = Any
const InnerFuncs = Dict{Any, Any}
const OuterFunc = Any
const TraitsStore = TypeDict{Tuple{OuterFunc, InnerFuncs}}
# here all the states are stored which are needed to realize @traits macro
const traits_store = Dict{Tuple{Symbol, Symbol}, TraitsStore}()

function getorcreatestore!(mod, funcname)
  storename = Symbol(funcname, "_traitsstore")
  key = (Symbol(string(mod)), funcname)
  store = if haskey(traits_store, key)
    traits_store[key]
  else
    newstore = TraitsStore()
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

struct _BetweenTypeVarsAndTraits end
struct _BetweenArgsAndTypeVars end
_innerfunctionname(funcname) = Symbol("_", funcname, "_traits")

function render(store::TraitsStore)
  exprs = []
  for (outerfunc, innerfuncs) in values(store)
    push!(exprs, render(outerfunc))
    for (fixed, nonfixed) in innerfuncs
      innerfunc = (fixed = fixed, nonfixed = nonfixed)
      push!(exprs, render(outerfunc, innerfunc))
    end
  end
  Expr(:block, exprs...)
end

function render(outerfunc_innerfuncs::Tuple{OuterFunc, InnerFuncs})
  outerfunc, innerfuncs = outerfunc_innerfuncs
  exprs = []
  push!(exprs, render(outerfunc))
  for (fixed, nonfixed) in innerfuncs
    innerfunc = (fixed = fixed, nonfixed = nonfixed)
    push!(exprs, render(outerfunc, innerfunc))
  end
  Expr(:block, exprs...)
end

function render(outerfunc_innerfunc::Tuple{OuterFunc, InnerFunc})
  outerfunc, innerfunc = outerfunc_innerfunc
  render(outerfunc, innerfunc)
end

function _map_args(new_to_old, innerargs)
  map(innerargs) do a
    # pure _ is currently buggy, see https://github.com/JuliaLang/julia/issues/32727
    # hence we use ::Any instead
    get(new_to_old, a, :(::Any))
  end
end

"""
render innerfunction
(this is only possible with informations from outerfunc)
"""
function render(outerfunc, innerfunc)
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
  innerfunc_parsed = Parsers.Function_Parsed(
    name = _innerfunctionname(outerfunc.fixed.name),
    curlies = [],
    args = args,
    kwargs = innerfunc.nonfixed.kwargs,
    wheres = [],
    body = innerfunc.nonfixed.body
  )
  toAST(innerfunc_parsed)
end

function render(outerfunc)
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
    name = _innerfunctionname(outerfunc.fixed.name),
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
  toAST(outerfunc_parsed)
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
  typevar_new_to_old = Dict{Symbol, Symbol}()
  arg_new_to_old = Dict{Symbol, Symbol}()
  arg_new_to_default = Dict{Symbol, Any}()

  countfrom_typevars = Base.Iterators.Stateful(Base.Iterators.countfrom(1))
  countfrom_args = Base.Iterators.Stateful(Base.Iterators.countfrom(1))

  # enumerate types
  # ---------------
  # we add _BetweenCurliesAndArgs to reuse ``type`` as identifying signature without mixing curly types and arg types
  typeexpr = Expr(:curly, Tuple, func_parsed.curlies...,
    _BetweenCurliesAndArgs, (toAST(a.type) for a in args_parsed)...)
  typeexpr_full = :($typeexpr where {$(toAST(func_parsed.wheres)...)})
  type = Base.eval(mod, typeexpr_full)
  # TODO make this simpler to read
  # TODO do not distinguish between first and second level,
  # but between three Type kinds and how they nest: Tuple, Vararg, other
  typebase, typevars = _split_typevar(type)
  typeexprs_new, typevars_new = normalize_typevars(typebase, typevars, typevar_new_to_old, countfrom_typevars)

  curlies_typeexprs_new = typeexprs_new[1:length(func_parsed.curlies)]
  args_typeexprs_new = typeexprs_new[length(func_parsed.curlies)+2:end]  # + 2 because of _BetweenCurliesAndArgs

  for (a, type) in zip(args_parsed, args_typeexprs_new)
    a.type = type

    # enumerate args
    # --------------
    arg_position = next(countfrom_args)
    new = normalized_arg_by_position(arg_position)
    arg_new_to_old[new] = a.name
    a.name = new
  end

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

normalized_typevar_by_position(position::Int) = TypeVar(Symbol("T", position))
normalized_typevar_by_position(position::Int, ub) = TypeVar(Symbol("T", position), ub)
normalized_typevar_by_position(position::Int, lb, ub) = TypeVar(Symbol("T", position), lb, ub)
normalized_arg_by_position(position::Int) = Symbol("a", position)

_split_typevar(base) = base, []
function _split_typevar(t::UnionAll)
  base, typevars = _split_typevar(t.body)
  base, [t.var; typevars...]
end

"""
traverses a given type, renaming all typeparameters to normalized name

returns type with substituted typevars + list of function level typevars
"""
function normalize_typevars(type, typevars_old, typevar_new_to_old, countfrom)
  typeexprs = []
  typevars = TypeVar[]
  for p in type.parameters
    _typeexpr, _typevars = _normalize_typevars1(p, typevars_old, typevar_new_to_old, countfrom)
    push!(typeexprs, _typeexpr)
    append!(typevars, _typevars)
  end
  typeexprs, typevars
end
# the very first level treats UnionAlls differently in that they will become function where typevariables
function _normalize_typevars1(type::UnionAll, typevars_old, typevar_new_to_old, countfrom)
  typebase, extra_typevars = _split_typevar(type)
  # if typebase.name.wrapper === Vararg
  if typebase.name.name == :Vararg
    # Vararg behave different than all other types when used as TypeVar for Tuple, namely
    # Tuple{Vararg{T} where T} != (Tuple{Vararg{T}} where T)
    # which is the same as all subsequent levels
    _normalize_typevars(type, typevars_old, typevar_new_to_old, countfrom)
  else
    _normalize_typevars(typebase, [typevars_old; extra_typevars], typevar_new_to_old, countfrom)
end
function _normalize_typevars1(type, typevars_old, typevar_new_to_old, countfrom)
  _normalize_typevars(type, typevars_old, typevar_new_to_old, countfrom)
end

function _normalize_typevars(type::DataType, typevars_old, typevar_new_to_old, countfrom::Base.Iterators.Stateful)
  if isabstracttype(type)
    # TODO how does this case behave recursively?
    i = next(countfrom)
    new = normalized_typevar_by_position(i, type)
    new.name, [new]
  else
    if isempty(type.parameters)
      type, TypeVar[]
    else
      # recurse
      typeexprs = []
      typevars = TypeVar[]
      for p in type.parameters
        _typeexpr, _typevars = _normalize_typevars(p, typevars_old, typevar_new_to_old, countfrom)
        push!(typeexprs, _typeexpr)
        append!(typevars, _typevars)
      end
      Expr(:curly, Base.unwrap_unionall(type).name.name, typeexprs...), typevars
    end
  end
end

function _normalize_typevars(tv::TypeVar, typevars_old, typevar_new_to_old, countfrom::Base.Iterators.Stateful)
  if tv in typevars_old
    i = next(countfrom)
    new = normalized_typevar_by_position(i, tv.lb, tv.ub)
    typevar_new_to_old[new.name] = tv.name
    new.name, [new]
  else  # this might be a nested UnionAll somewhere
    tv.name, TypeVar[]
  end
end
function _normalize_typevars(type::UnionAll, typevars_old, typevar_new_to_old, countfrom)
  typebase, typevars_unionall = _split_typevar(type)
  typebase_expr_new, typevars_new = _normalize_typevars(typebase, typevars_old, typevar_new_to_old, countfrom)
  _expr_rewrap_typevars(typebase_expr_new, typevars_unionall), typevars_new
end
_normalize_typevars(any, typevars_old, typevar_new_to_old, countfrom) = any, TypeVar[]

function _expr_rewrap_typevars(typeexpr::Expr, typevars)
  Expr(:where, typeexpr, toAST.(typevars)...)
end

end # module
