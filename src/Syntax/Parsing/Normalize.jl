module Normalize
export normalize_func, normalized_arg_by_position

using SimpleMatch: @match
using WhereTraits: InternalState
using WhereTraits.Utils: MacroEnv, next!
using WhereTraits.Utils: split_typevar, normalize_typevars, normalize_mod_and_name, normalized_arg_by_position
using ExprParsers

# As part of the parsing, we need to normalization the outer function
# -------------------------------------------------------------------

struct _BetweenCurliesAndArgs end
Base.show(io::IO, x::Type{<:_BetweenCurliesAndArgs}) = print(io, "<CURLIES|SIGNATURE|ARGS>")


normalize_func(env::MacroEnv, func_expr::Expr) = normalize_func(env, EP.Function()(func_expr))

function normalize_func(env::MacroEnv, func_parsed::EP.Function_Parsed)
    args_parsed = [parse_expr(EP.Arg(), a) for a in func_parsed.args]
    @assert all(arg -> arg.default isa EP.NoDefault, args_parsed) "Can only normalize functions without positional default arguments"


    # normalize types
    # ---------------

    # we add _BetweenCurliesAndArgs to reuse `type` as identifying signature without mixing curly types and arg types
    typeexpr = to_expr(:(
        Tuple{$(func_parsed.curlies...), $_BetweenCurliesAndArgs, $((a.type for a in args_parsed)...)}
        where {$(func_parsed.wheres...)}
    ))
    type = Base.eval(env.mod, typeexpr)
    typebase, typevars_old = split_typevar(type)  # typebase == Tuple{P1, P2, P3, ...}
    typeexprs_new, typevars_new, typevar_new_to_old = normalize_typevars(typebase.parameters, typevars_old)

    curlies_typeexprs_new = typeexprs_new[1:length(func_parsed.curlies)]
    args_typeexprs_new = typeexprs_new[length(func_parsed.curlies)+2:end]  # + 2 because of _BetweenCurliesAndArgs

    # we need to normalize the signature type because of a Julia Unreachable Reached
    # see https://github.com/JuliaLang/julia/issues/44339
    typeexpr_normalized = to_expr(:(
        Tuple{$(curlies_typeexprs_new...), $_BetweenCurliesAndArgs, $(args_typeexprs_new...)}
        where {$(typevars_new...)}
    ))
    type_normalized = Base.eval(env.mod, typeexpr_normalized)


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


    # dropped typevariables
    # ---------------------

    simple_where_parser = EP.AnyOf(EP.anysymbol, EP.TypeRange())
    typevars_old = map(func_parsed.wheres) do wh
        parsed = parse_expr(simple_where_parser, to_expr(wh))
        @match(parsed) do f
            f(x::Symbol) = x
            f(x::EP.TypeRange_Parsed) = x.name
        end
    end
    typevars_kept = values(typevar_new_to_old)
    typevars_dropped = [v for v in typevars_old if v ∉ typevars_kept]


    # return
    # ------

    mod_original, funcname_original = normalize_mod_and_name(env.mod, func_parsed.name)
    
    # we return the whole signature to easily decide whether the function overwrites another
    outerfunc_fixed = InternalState.DefOuterFuncFixedPart{type_normalized}(
        # everything under fixed should be identifiable via signature
        signature = type_normalized,
        mod = mod_original,
        name = funcname_original,
        curlies = curlies_typeexprs_new,
        args = args_parsed,
        wheres = typevars_new,
        # body information
        innerargs_args = sort([a.name for a in args_parsed]),
        innerargs_typevars = sort([tv.name for tv in typevars_new]),
    )
    # this innerfunc_fixed is only an intermediate state, hence we fallback to using plain namedtuples
    innerfunc_fixed = (
        args_mapping = arg_new_to_old,
        typevars_mapping = typevar_new_to_old,
    )
    (; outerfunc_fixed, innerfunc_fixed, typevars_dropped)
end

end  # module