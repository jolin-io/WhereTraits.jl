# Syntax Parser
# =============
module Parsing
export parse_traitsfunction
import WhereTraits
using WhereTraits.Utils
using SimpleMatch
using ExprParsers
include("NormalizeType.jl")
using .NormalizeType

parse_traitsfunction(env, expr::Expr, expr_original; kwargs...) = parse_traitsfunction(env, EP.Function()(expr), expr_original; kwargs...)
function parse_traitsfunction(env, func_parsed::EP.Function_Parsed, expr_original; on_traits_dropped = msg -> throw(ArgumentError(msg)))
  # Parse main syntax
  # =================
  args_names = [parse_expr(EP.Arg(), arg).name for arg in func_parsed.args]
  args_names_matcher = EP.AnyOf(args_names...)
  # TODO we could also allow dispatch on keyword arguments
  # kwargs_names = [EP.Arg()(kwarg).name for kwarg in func_parsed.kwargs]
  # kwargs_names_matcher = EP.AnyOf(kwargs_names...)

  whereexpr_parser = EP.AnyOf(
    # we allow dispatch on arguments
    EP.Named{:arg}(args_names_matcher), # interpreted as bool
    EP.Named{:arg}(EP.TypeAnnotation(name=args_names_matcher)),
    EP.Named{:arg}(EP.TypeRange(name=args_names_matcher)),

    # TODO we could also allow dispatch on keyword arguments
    # How? we store all kwargs per signature and then define traitfunctions like `func(kwargs[:b])`
    # EP.Named{:kwarg}(kwargs_names_matcher), # interpreted as bool
    # EP.Named{:kwarg}(EP.TypeAnnotation(name=kwargs_names_matcher)),
    # EP.Named{:kwarg}(EP.TypeRange(name=kwargs_names_matcher)),

    # all other symbols refer to standard TypeVariables
    # TypeAnnotation on TypeVar make only sense in the special case that the typevariable can take different types of binary type
    # both Symbol and TypeRange are normal where syntax, TypeAnnotation is not
    EP.Named{:normal}(EP.anysymbol),
    EP.Named{:typevar}(EP.TypeAnnotation(name=EP.anysymbol)),
    EP.Named{:normal}(EP.TypeRange(name=EP.anysymbol)),

    # we further allow to dispatch on functions from either args, kwargs, or standard typevars
    # which should be all the rest
    EP.Named{:func}(EP.Call()),  # interpreted as bool
    EP.Named{:func}(EP.TypeAnnotation()),
    EP.Named{:func}(EP.TypeRange()),
  )
  parsed_wheres = [parse_expr(whereexpr_parser, w) for w in func_parsed.wheres]
  normal_wheres, extra_wheres = filtersplit(parsed_wheres) do x
    x isa EP.Named{:normal}
  end

  # prepare extra wheres
  # ====================

  traits = map(extra_wheres) do expr
    @match(expr) do f
      # Bool values are lifted to typelevel BoolType
      # plain arguments are interpreted as bool
      f(x::EP.Named{:arg, Symbol}) = :($BoolType($(to_expr(x.value))))
      # plain calls are assumed to refer to boolean expressions
      function f(x::EP.Named{:func, EP.Call_Parsed})
        if x.value.name == :!
          # if the last call is negation "!" we take the thing which is negated
          # as a trait function and dispatch on False
          :($BoolType($(to_expr(x.value.args[1]))))
        else
          :($BoolType($(to_expr(x.value))))
        end
      end
      # lifting :: dispatch to typelevel <: dispatch
      f(x::EP.Named{<:Any, EP.TypeAnnotation_Parsed}) = :(typeof($(to_expr(x.value.name))))
      # standard typelevel <: dispatch
      f(x::EP.Named{<:Any, EP.TypeRange_Parsed}) = to_expr(x.value.name)
    end
  end

  traits_matching_types = map(enumerate(extra_wheres)) do (i, expr)
    @match(expr) do f
      f(x::EP.Named{:arg, Symbol}) = True  # plain arguments are interpreted as bool
      f(x::EP.Named{:func, EP.Call_Parsed}) = (x.value.name == :!) ? :(Type{<:$False}) : :(Type{<:$True})  # plain calls are assumed to refer to boolean expressions
      f(x::EP.Named{<:Any, EP.TypeAnnotation_Parsed}) = :(Type{<:$(to_expr(x.value.type))})
      function f(x::EP.Named{<:Any, EP.TypeRange_Parsed})
        tr = x.value
        @assert !(tr.lb === Union{} && tr.ub == Any) "should have at least an upperbound or a lowerbound"
        if tr.lb === Union{}  # only upperbound
          :(Type{<:$(to_expr(tr.ub))})
        elseif tr.ub === Any  # only LowerBound
          :(Type{>:$(to_expr(tr.lb))})
        else  # both
          :(Type{T} where {$(to_expr(tr.lb)) <: T <: $(to_expr(tr.up))})
        end
      end
    end
  end

  # normalize first function dispatch level
  # =======================================

  outerfunc_parsed = EP.Function_Parsed(
    name = func_parsed.name,
    curlies = func_parsed.curlies,
    args = func_parsed.args,
    kwargs = [],
    # we need to use to_expr here, as normalize_func expects a
    # standard Function_Parsed, where wheres is a list of Expr
    wheres = to_expr(normal_wheres),
    body = :nothing
  )

  outerfunc_fixed, innerfunc_fixed = normalize_func(env, outerfunc_parsed)

  @assert(
    isempty(intersect(keys(innerfunc_fixed.args_mapping), keys(innerfunc_fixed.typevars_mapping))),
    "args and typevariables must not overlap!
    Found $(intersect(keys(innerfunc_fixed.args_mapping), keys(innerfunc_fixed.typevars_mapping))) in both.")

  typevars_old = map(normal_wheres) do parsed
    @match(parsed) do f
      f(x::EP.Named{:normal, Symbol}) = x.value
      f(x::EP.Named{:normal, EP.TypeRange_Parsed}) = x.value.name
    end
  end
  kept_typevars_old = values(innerfunc_fixed.typevars_mapping)
  dropped_typevars_old = [v for v in typevars_old if v ∉ kept_typevars_old]

  traits_filtered = filter(t -> !depends_on(t, dropped_typevars_old), traits)
  if length(traits) != length(traits_filtered)
    traits_dropped = setdiff(traits, traits_filtered)
    on_traits_dropped("Given traits depend on droppable typeparameters ($dropped_typevars_old). WhereTraits: $traits_dropped")
  end

  old_to_new = Dict(v => k for (k, v) in merge(innerfunc_fixed.args_mapping, innerfunc_fixed.typevars_mapping))
  # TODO we currently do not normalize the traits function names
  # TODO e.g. using both `Base.IteratorSize(a)` and `IteratorSize(a)` result in two different traits currently
  traits_normalized = _change_symbols(traits_filtered, old_to_new)
  # we map the traitsdefinition to a signature including a name for easier debugging and the correct type
  traits_mapping = Dict(k => Expr(:(::), Symbol("'", k, "'"), v)
                        for (k, v) in zip(traits_normalized, traits_matching_types))
  # we may encounter no traits at all, namely in the case where a default clause is defined
  innerargs_traits = isempty(traits_normalized) ? [] : sortexpr(unique(traits_normalized))
  outerfunc′ = WhereTraits.InternalState.DefOuterFunc(
    outerfunc_fixed,
    WhereTraits.InternalState.DefOuterFuncNonFixedPart(innerargs_traits),
  )
  innerfunc′ = WhereTraits.InternalState.DefInnerFunc(
    WhereTraits.InternalState.DefInnerFuncFixedPart(
      args_mapping = innerfunc_fixed.args_mapping,
      typevars_mapping = innerfunc_fixed.typevars_mapping,
      traits_mapping = traits_mapping,
    ),
    WhereTraits.InternalState.DefInnerFuncNonFixedPart(
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

normalize_func(env::MacroEnv, func_expr::Expr) = normalize_func(env, EP.Function()(func_expr))
function normalize_func(env::MacroEnv, func_parsed::EP.Function_Parsed)
  args_parsed = [parse_expr(EP.Arg(), a) for a in func_parsed.args]
  @assert all(arg -> arg.default isa EP.NoDefault, args_parsed) "Can only normalize functions without positional default arguments"

  # normalize types
  # ---------------
  # we add _BetweenCurliesAndArgs to reuse `type` as identifying signature without mixing curly types and arg types
  typeexpr = Expr(:curly, Tuple, func_parsed.curlies...,
    _BetweenCurliesAndArgs, (to_expr(a.type) for a in args_parsed)...)
  typeexpr_full = :($typeexpr where {$(to_expr(func_parsed.wheres)...)})
  type = Base.eval(env.mod, typeexpr_full)
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
  # TODO we use call by position
  # TODO because call by keyword leads to UnreachableReached, see https://github.com/JuliaLang/julia/issues/35698
  outerfunc_fixed = WhereTraits.InternalState.DefOuterFuncFixedPart{type}(
    # everything under fixed should be identifiable via signature
    type,
    func_parsed.name,
    curlies_typeexprs_new,
    args_parsed,
    typevars_new,
    # body information
    sort([a.name for a in args_parsed]),
    sort([tv.name for tv in typevars_new]),
  )
  # outerfunc_fixed = WhereTraits.InternalState.DefOuterFuncFixedPart{type}(
  #   # everything under fixed should be identifiable via signature
  #   signature = type,
  #   name = func_parsed.name,
  #   curlies = curlies_typeexprs_new,
  #   args = args_parsed,
  #   wheres = typevars_new,
  #   # body information
  #   innerargs_args = sort([a.name for a in args_parsed]),
  #   innerargs_typevars = sort([tv.name for tv in typevars_new]),
  # )
  # this innerfunc_fixed is only an intermediate state, hence we fallback to using plain namedtuples
  innerfunc_fixed = (
    args_mapping = arg_new_to_old,
    typevars_mapping = typevar_new_to_old,
  )
  outerfunc_fixed, innerfunc_fixed
end

normalized_arg_by_position(position::Int) = Symbol("a", position)

end # module
