module Parsing
export parse_traitsfunction, parse_traitsorder

using WhereTraits: InternalState
using WhereTraits.Utils
using SimpleMatch
using ExprParsers

include("Normalize.jl")
using .Normalize

using Graphs
using MetaGraphs


# parse_traitsfunction
# ====================

parse_traitsfunction(env, expr::Expr, expr_original; kwargs...) = parse_traitsfunction(env, EP.Function()(expr), expr_original; kwargs...)

function parse_traitsfunction(env, func_parsed::EP.Function_Parsed, expr_original; on_traits_dropped = msg -> throw(ArgumentError(msg)))
    
    # Parse main syntax
    # -----------------
    
    args_names = [parse_expr(EP.Arg(), arg).name for arg in func_parsed.args]
    
    # TODO we could also allow dispatch on keyword arguments
    # kwargs_names = [EP.Arg()(kwarg).name for kwarg in func_parsed.kwargs]
    # kwargs_names_matcher = EP.AnyOf(kwargs_names...)

    whereexpr_parser = create_where_parser(args_names)
    parsed_wheres = [parse_expr(whereexpr_parser, w) for w in func_parsed.wheres]
    normal_wheres, extra_wheres = filtersplit(parsed_wheres) do x
        x isa EP.Named{:normal}
    end


    # normalize first function dispatch level
    # ---------------------------------------

    outerfunc_parsed = EP.Function_Parsed(
        name = func_parsed.name,
        curlies = func_parsed.curlies,
        args = func_parsed.args,
        kwargs = [],
        # we need to use to_expr here, as normalize_func expects a
        # standard Function_Parsed, where wheres is a list of Expr
        wheres = normal_wheres,
        body = :nothing
    )

    outerfunc_fixed, innerfunc_fixed, typevars_dropped = normalize_func(env, outerfunc_parsed)

    @assert(
        isempty(intersect(keys(innerfunc_fixed.args_mapping), keys(innerfunc_fixed.typevars_mapping))),
        "args and typevariables must not overlap!
        Found $(intersect(keys(innerfunc_fixed.args_mapping), keys(innerfunc_fixed.typevars_mapping))) in both.")


    # prepare extra wheres
    # --------------------

    traits_names = map(traitname_from_parsedtrait, extra_wheres)
    traits_args = map(traitarg_from_parsedtrait, extra_wheres)
    traits_upperbounds = map(traitupperbound_from_parsedtrait, extra_wheres)


    # deal with dropped typevariables 
    # -------------------------------

    filtered = [!depends_on(t, typevars_dropped) for t in traits_names]
    traits_names_filtered = traits_names[filtered]
    traits_args_filtered = traits_args[filtered]
    traits_upperbounds_filtered = traits_upperbounds[filtered]

    if length(traits_names) != length(traits_names_filtered)
        traits_dropped = traits_names[.!filtered]
        on_traits_dropped("Given traits depend on droppable typeparameters ($typevars_dropped). WhereTraits: $traits_dropped")
    end


    # normalized naming also for innerfunc body
    # -----------------------------------------

    old_to_new = Dict(v => k for (k, v) in merge(innerfunc_fixed.args_mapping, innerfunc_fixed.typevars_mapping))
    # TODO we currently do not normalize the traits function names
    # TODO e.g. using both `Base.IteratorSize(a)` and `IteratorSize(a)` result in two different traits 
    traits_names_filtered_normalized = change_symbols(old_to_new, traits_names_filtered) 
    traits_args_filtered_normalized = change_symbols(old_to_new, traits_args_filtered)

    traits_mapping = Dict(k => :(::Type{<:$v})
        for (k, v) in zip(traits_names_filtered_normalized, traits_upperbounds_filtered))

    innerargs_traits_mapping = Dict(k => v
        for (k, v) in zip(traits_names_filtered_normalized, traits_args_filtered_normalized))

    @assert length(traits_mapping) == length(traits_names_filtered_normalized) "FATAL. Found duplicate traits in $(traits_names_filtered_normalized)"
    
    # we may encounter no traits at all, namely in the case where a default clause is defined
    traits = sortexpr(traits_names_filtered_normalized)


    # bring everything together
    # -------------------------

    outerfunc = InternalState.DefOuterFunc(
        outerfunc_fixed,
        InternalState.DefOuterFuncNonFixedPart(
            traits,
            innerargs_traits_mapping,
        ),
    )
    innerfunc = InternalState.DefInnerFunc(
        InternalState.DefInnerFuncFixedPart(
            args_mapping = innerfunc_fixed.args_mapping,
            typevars_mapping = innerfunc_fixed.typevars_mapping,
            traits_mapping = traits_mapping,
        ),
        InternalState.DefInnerFuncNonFixedPart(
            mod = env.mod,
            kwargs = func_parsed.kwargs,
            body = func_parsed.body,
            expr_original = expr_original,
        ),
    )
    (; outerfunc, innerfunc)
end


# CAUTION: create_where_parser defines EP.Named{:name} which naming is important for all other functions
function create_where_parser(args_names)
    args_names_matcher = EP.AnyOf(args_names...)

    return EP.AnyOf(
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
end

function traitname_from_parsedtrait(expr)
    @match(expr) do f
        # Bool values are lifted to typelevel BoolType
        # plain arguments are interpreted as bool
        f(x::EP.Named{:arg, Symbol}) = to_expr(x.value)
        # plain calls are assumed to refer to boolean expressions
        function f(x::EP.Named{:func, EP.Call_Parsed})
            if x.value.name == :!
                # if the last call is negation "!" we take the thing which is negated
                # as a trait function and dispatch on False
                to_expr(x.value.args[1])
            else
                to_expr(x.value)
            end
        end
        # lifting :: dispatch to typelevel <: dispatch
        f(x::EP.Named{<:Any, EP.TypeAnnotation_Parsed}) = to_expr(x.value.name)
        # standard typelevel <: dispatch
        f(x::EP.Named{<:Any, EP.TypeRange_Parsed}) = to_expr(x.value.name)
    end
end


function traitarg_from_parsedtrait(expr)
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


function traitupperbound_from_parsedtrait(expr)
    @match(expr) do f
        f(x::EP.Named{:arg, Symbol}) = True  # plain arguments are interpreted as bool
        f(x::EP.Named{:func, EP.Call_Parsed}) = (x.value.name == :!) ? False : True  # plain calls are assumed to refer to boolean expressions
        f(x::EP.Named{<:Any, EP.TypeAnnotation_Parsed}) = to_expr(x.value.type)
        function f(x::EP.Named{<:Any, EP.TypeRange_Parsed})
            tr = x.value
            @assert !(tr.lb === Union{} && tr.ub == Any) "should have at least an upperbound or a lowerbound"
            if tr.lb === Union{}  # only upperbound
                to_expr(tr.ub)
            else # LowerBound
                error("@traits does not support dispatch on lowerbounds, found lowerbound $(tr.lb)")
            end
        end
    end
end


# parse_traitsorder
# =================


parse_traitsorder(env, expr::Expr, expr_original; kwargs...) = parse_traitsorder(env, EP.Function()(expr), expr_original; kwargs...)

function parse_traitsorder(env, func_parsed::EP.Function_Parsed, expr_original; on_traits_dropped = msg -> throw(ArgumentError(msg)))

    # normalize first function dispatch level
    # ---------------------------------------

    outerfunc_parsed = EP.Function_Parsed(
        name = func_parsed.name,
        curlies = func_parsed.curlies,
        args = func_parsed.args,
        kwargs = [],
        # we need to use to_expr here, as normalize_func expects a
        # standard Function_Parsed, where wheres is a list of Expr
        wheres = func_parsed.wheres,
        body = :nothing
    )

    outerfunc_fixed, innerfunc_fixed, typevars_dropped = normalize_func(env, outerfunc_parsed)

    @assert(
        isempty(intersect(keys(innerfunc_fixed.args_mapping), keys(innerfunc_fixed.typevars_mapping))),
        "args and typevariables must not overlap!
        Found $(intersect(keys(innerfunc_fixed.args_mapping), keys(innerfunc_fixed.typevars_mapping))) in both.")


    # collect traits
    # --------------

    traits_parser = EP.AnyOf(EP.anysymbol, EP.Call())  # we expect plain names or function calls, no TypeAnnotations or TypeRanges
    traits_parsed = [parse_expr(traits_parser, e) for e in func_parsed.body.args if !isa(e, LineNumberNode)]
    traits = map(to_expr, traits_parsed)


    # deal with dropped typevariables 
    # -------------------------------
    
    traits_filtered = filter(t -> !depends_on(t, typevars_dropped), traits)
    
    if length(traits) != length(traits_filtered)
        traits_dropped = setdiff(traits, traits_filtered)
        on_traits_dropped("Given traits depend on droppable typeparameters ($typevars_dropped). WhereTraits: $traits_dropped")
    end
    

    # normalize traits
    # ----------------

    old_to_new = Dict(v => k for (k, v) in merge(innerfunc_fixed.args_mapping, innerfunc_fixed.typevars_mapping))
    # TODO we currently do not normalize the traits function names
    # TODO e.g. using both `Base.IteratorSize(a)` and `IteratorSize(a)` result in two different traits currently
    traits_filtered_normalized = change_symbols(old_to_new, traits_filtered)

    
    # return 
    # ------

    disambiguation = InternalState.DefDisambiguation(traits_filtered_normalized; mod = env.mod, expr_original = expr_original)
    
    return (;
        outerfunc_fixed,
        disambiguation,
    )
end

end # module