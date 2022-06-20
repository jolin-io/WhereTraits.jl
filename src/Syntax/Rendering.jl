module Rendering
export RenderType, RenderTraitsStore, RenderOuterFunc, RenderInnerFuncs, RenderInnerFunc, RenderDisambiguation, RenderDoc
export render, render_all 

using WhereTraits: WhereTraits, CONFIG
using WhereTraits.Utils
using WhereTraits.Utils: Ambiguities
using WhereTraits.InternalState

using Base: @kwdef
using ExprParsers
using Markdown
using Setfield
using SimpleMatch
using Pipe
using StructEquality

abstract type RenderType end

@struct_hash_equal @kwdef struct RenderTraitsStore{Signature} <: RenderType
    store::InternalState.TraitsStore{Signature}
end

@struct_hash_equal @kwdef struct RenderInnerFuncs{Signature} <: RenderType
    outer::InternalState.DefOuterFunc{Signature}
    inners::InternalState.DefInnerFuncs
end
@struct_hash_equal @kwdef struct RenderOuterFunc{Signature} <: RenderType
    outer::InternalState.DefOuterFunc{Signature}
end
@struct_hash_equal @kwdef struct RenderInnerFunc{Signature} <: RenderType
    # we need the outerfunc to construct the innerfunc, as it depends on the ordering of the traits functions
    # which are defined in the outerfunc
    outer::InternalState.DefOuterFunc{Signature}
    inner::InternalState.DefInnerFunc
end

# TODO probably this needs a removal of old methods if rerendered 
@struct_hash_equal @kwdef struct RenderDisambiguation{Signature} <: RenderType
    outer::InternalState.DefOuterFunc{Signature}
    inners::InternalState.DefInnerFuncs
    disambiguation::InternalState.DefDisambiguation
end
function RenderDisambiguation(store::InternalState.TraitsStore)
    RenderDisambiguation(
        store.outerfunc,
        store.innerfuncs,
        store.disambiguation,
    )
end

@struct_hash_equal @kwdef struct RenderDoc{Signature} <: RenderType
    outer::InternalState.DefOuterFunc{Signature}
    inners::InternalState.DefInnerFuncs
    inner::InternalState.DefInnerFunc
end



# Render
# ======


function render(env::MacroEnv, torender::RenderTraitsStore)
    outerfunc = torender.store.outerfunc

    # if we are rendering code for the same module, we need to drop the module information
    # this is needed for defining the function the very first time as `MyModule.func(...) = ...` is invalid syntax
    # for the initial definition  
    name = if env.mod === outerfunc.fixed.mod
        outerfunc.fixed.name
    else
        :($(outerfunc.fixed.mod).$(outerfunc.fixed.name))
    end

    :(function $(to_expr(name))(::WhereTraits.InternalState.TraitsStoreSingleton, ::Type{$(outerfunc.fixed.signature)})
        $(torender.store)
    end)
end


"""
render a whole TraitsStore

for debugging purposes only
"""
function render_all(env::MacroEnv, store::InternalState.TraitsStore)
    exprs = []
    outer, inners, disambiguation = store.outerfunc, store.innerfuncs, store.disambiguation
    push!(exprs, render(env, RenderOuterFunc(outer)))
    push!(exprs, render(env, RenderDisambiguation(outer, inners, disambiguation)))
    for (fixed, nonfixed) in innerfuncs
        innerfunc = (fixed = fixed, nonfixed = nonfixed)
        push!(exprs, render(env, RenderInnerFunc(outerfunc, innerfunc)))
    end

    flatten_blocks(Expr(:block, exprs...))
end


"""
render several RenderType at once
"""
function render(env::MacroEnv, torender_several::Vector)
    exprs = map(torender_several) do torender
        render(env, torender)
    end
    flatten_blocks(Expr(:block, exprs...))
end


"""
rerender one single outerfunc and respective innerfuncs
"""
function render(env::MacroEnv, torender::RenderInnerFuncs)
    outerfunc, innerfuncs = torender.outer, torender.inners
    exprs = []
    for (fixed, nonfixed) in innerfuncs
        innerfunc = InternalState.DefInnerFunc(fixed = fixed, nonfixed = nonfixed)
        push!(exprs, render(env, RenderInnerFunc(outerfunc, innerfunc)))
    end
    flatten_blocks(Expr(:block, exprs...))
end


"""
render innerfunction
(this is only possible with informations from outerfunc)
"""
function render(env::MacroEnv, torender::RenderInnerFunc)
    outerfunc, innerfunc = torender.outer, torender.inner
    args = [
        :(::$(InternalState.TraitsDefSingleton));
        # we need to dispatch on the signature so that different outerfuncs don't
        # overwrite each other's innerfunc
        :(::$(Type{outerfunc.fixed.signature}));
        _map_args(innerfunc.fixed.args_mapping, outerfunc.fixed.innerargs_args);
        :(::$(InternalState.ArgsHelpers_BetweenArgsAndTypeVars));
        _map_args(innerfunc.fixed.typevars_mapping, outerfunc.fixed.innerargs_typevars);
        :(::$(InternalState.ArgsHelpers_BetweenTypeVarsAndTraits));
        _map_traits(innerfunc.fixed.traits_mapping, outerfunc.nonfixed.traits);
    ]

    # if we are rendering code for the same module, we need to drop the module information
    # this is needed for defining the function the very first time as `MyModule.func(...) = ...` is invalid syntax
    # for the initial definition
    name = if env.mod === outerfunc.fixed.mod
        outerfunc.fixed.name
    else
        :($(outerfunc.fixed.mod).$(outerfunc.fixed.name))
    end


    innerfunc_parsed = EP.Function_Parsed(
        name = name,
        curlies = [],
        args = args,
        kwargs = innerfunc.nonfixed.kwargs,
        wheres = [],
        body = innerfunc.nonfixed.body
    )
    to_expr(innerfunc_parsed)
end

function _map_args(new_to_old, innerargs)
    map(innerargs) do a
        # pure `_` is currently buggy, see https://github.com/JuliaLang/julia/issues/32727
        # hence we use ::Any instead
        get(new_to_old, a, Expr(:(::), Symbol("'", a, "'"), :(Any)))
    end
end

function _map_traits(new_to_old, innerargs)
    map(innerargs) do a
        type_dispatch = get(new_to_old, a, :(::Type{<:Any}))
        # we map the traitsdefinition to a signature including a name for easier debugging and the correct type
        if EP.isexpr(type_dispatch, :(::)) && length(type_dispatch.args) == 1  # unnamed `::MyType` 
            Expr(:(::), Symbol("'", a, "'"), type_dispatch.args[1])
        else
            type_dispatch
        end
    end
end

"""
render outer function
"""
function render(env::MacroEnv, torender::RenderOuterFunc)
    outerfunc = torender.outer

    innerargs = [
        #TODO InternalState.traitsdisambiguationsingleton;
        InternalState.traitsdisambiguationsingleton;
        # we need to dispatch on the signature so that different outerfuncs don't
        # overwrite each other's innerfunc
        outerfunc.fixed.signature;
        outerfunc.fixed.innerargs_args;
        InternalState.ArgsHelpers_BetweenArgsAndTypeVars();
        outerfunc.fixed.innerargs_typevars;
        InternalState.ArgsHelpers_BetweenTypeVarsAndTraits();
        map(t -> outerfunc.nonfixed.innerargs_traits_mapping[t], outerfunc.nonfixed.traits);
    ]
    innerfunc_call = EP.Call_Parsed(
        name = :($(outerfunc.fixed.mod).$(outerfunc.fixed.name)),
        curlies = [],
        args = innerargs,
        kwargs = [:(kwargs...)],
    )
    # Rewrite Traits MethodErrors
    innerfunc_call_wrapped = quote
        try
            $innerfunc_call
        catch exc
            $(InternalState.isWhereTraitsMethodError)(exc) || rethrow()
            throw($(InternalState.WhereTraitsMethodError)(exc))
        end
    end

    # add LineNumberNode for debugging purposes
    body = Expr(:block, env.source, innerfunc_call_wrapped)

    # if we are rendering code for the same module, we need to drop the module information
    # this is needed for defining the function the very first time as `MyModule.func(...) = ...` is invalid syntax
    # for the initial definition
    name = if env.mod === outerfunc.fixed.mod
        outerfunc.fixed.name
    else
        :($(outerfunc.fixed.mod).$(outerfunc.fixed.name))
    end

    outerfunc_parsed = EP.Function_Parsed(
        name = name,
        curlies = outerfunc.fixed.curlies,
        args = outerfunc.fixed.args,
        kwargs = [:(kwargs...)],
        wheres = outerfunc.fixed.wheres,
        body = body,
    )
    to_expr(outerfunc_parsed)
end


"""
render outer function
"""
function render(env::MacroEnv, torender::RenderDisambiguation)
    outerfunc, innerfuncs, traits_order = torender.outer, torender.inners, torender.disambiguation.traits_order

    # computing disambiguations
    # -------------------------

    traits = outerfunc.nonfixed.traits

    dispatches = map(collect(innerfuncs)) do (fixed, nonfixed)
        map(traits) do trait
            expr_type_dispatch = get(fixed.traits_mapping, trait, :(::Type))
            expr_type = parse_expr(EP.TypeAnnotation(), expr_type_dispatch)
            type = Base.eval(nonfixed.mod, expr_type.type)
            @assert !isa(type, Union) "Union are not supported as upperbounds. This might be changed in the future. As a workaround define the function several times for each Union type separately."
            type.var.ub
        end
    end

    ambiguities = Ambiguities.ambiguities(dispatches)

    # rendering disambiguation functions
    # ----------------------------------

    # if we are rendering code for the same module, we need to drop the module information
    # this is needed for defining the function the very first time as `MyModule.func(...) = ...` is invalid syntax
    # for the initial definition
    name = if env.mod === outerfunc.fixed.mod
        outerfunc.fixed.name
    else
        :($(outerfunc.fixed.mod).$(outerfunc.fixed.name))
    end

    exprs = map(collect(ambiguities)) do ambiguity

        resolution = Ambiguities.resolve(ambiguity, traits, traits_order)

        body = @match(resolution) do f
            
            function f(resolution::Ambiguities.Resolution)
                resolution_traits = dispatches[resolution.i_method]
                
                innerargs = [
                    InternalState.traitsdefsingleton;
                    # we need to dispatch on the signature so that different outerfuncs don't
                    # overwrite each other's innerfunc
                    outerfunc.fixed.signature;
                    outerfunc.fixed.innerargs_args;
                    InternalState.ArgsHelpers_BetweenArgsAndTypeVars();
                    outerfunc.fixed.innerargs_typevars;
                    InternalState.ArgsHelpers_BetweenTypeVarsAndTraits();
                    resolution_traits;
                ]

                return EP.Call_Parsed(
                    name = :($(outerfunc.fixed.mod).$(outerfunc.fixed.name)),
                    curlies = [],
                    args = innerargs,
                    kwargs = [:(kwargs...)],
                )
            end
            
            function f(conflict::Ambiguities.NoResolution)
                traits_conflicting = [traits[i] for i ∈ conflict.indices_traits_conflicting]
                
                call = to_expr(EP.Call_Parsed(
                    name    = :($(outerfunc.fixed.mod).$(outerfunc.fixed.name)),
                    curlies = outerfunc.fixed.curlies,
                    args    = outerfunc.fixed.args,
                    kwargs  = [],
                ))
                wheres = isempty(outerfunc.fixed.wheres) ? call : Expr(:where, call, to_expr.(outerfunc.fixed.wheres)...)
                exception = InternalState.WhereTraitsAmbiguityError(traits_conflicting, wheres)
                return :(throw($exception))
            end
        end

        outerargs_traits = [Expr(:(::), Symbol("'", name, "'"), :(Type{<:$type}))
            for (name, type) in zip(outerfunc.nonfixed.traits, ambiguity.dispatch_resolution)]

        outerargs = [
            :(::$(InternalState.TraitsDisambiguationSingleton));
            # we need to dispatch on the signature so that different outerfuncs don't
            # overwrite each other's innerfunc
            :(::$(Type{outerfunc.fixed.signature}));
            outerfunc.fixed.innerargs_args;
            :(::$(InternalState.ArgsHelpers_BetweenArgsAndTypeVars));
            outerfunc.fixed.innerargs_typevars;
            :(::$(InternalState.ArgsHelpers_BetweenTypeVarsAndTraits));
            outerargs_traits;
        ]

        return EP.Function_Parsed(
            name = name,
            curlies = [],
            args = outerargs,
            kwargs = [:(kwargs...)],
            wheres = [],
            body = body
        ) |> to_expr
    end


    # rendering generic all-pass-through function
    # -------------------------------------------

    expr_generic = begin
        innerargs_traits = map(Counter(), outerfunc.nonfixed.traits) do i, trait
            Symbol("'trait_$(i)_$(trait)'")
        end
        outerargs_traits = [Expr(:(::), name, Any) for name in innerargs_traits]

        innerargs = [
            InternalState.traitsdefsingleton;
            # we need to dispatch on the signature so that different outerfuncs don't
            # overwrite each other's innerfunc
            outerfunc.fixed.signature;
            outerfunc.fixed.innerargs_args;
            InternalState.ArgsHelpers_BetweenArgsAndTypeVars();
            outerfunc.fixed.innerargs_typevars;
            InternalState.ArgsHelpers_BetweenTypeVarsAndTraits();
            innerargs_traits;
        ]

        body = EP.Call_Parsed(
            name = :($(outerfunc.fixed.mod).$(outerfunc.fixed.name)),
            curlies = [],
            args = innerargs,
            kwargs = [:(kwargs...)],
        )

        outerargs = [
            :(::$(InternalState.TraitsDisambiguationSingleton));
            # we need to dispatch on the signature so that different outerfuncs don't
            # overwrite each other's innerfunc
            :(::$(Type{outerfunc.fixed.signature}));
            outerfunc.fixed.innerargs_args;
            :(::$(InternalState.ArgsHelpers_BetweenArgsAndTypeVars));
            outerfunc.fixed.innerargs_typevars;
            :(::$(InternalState.ArgsHelpers_BetweenTypeVarsAndTraits));
            outerargs_traits;
        ]

        EP.Function_Parsed(
            name = name,
            curlies = [],
            args = outerargs,
            kwargs = [:(kwargs...)],
            wheres = [],
            body = body
        ) |> to_expr
    end


    # delete all previous methods
    # ---------------------------

    if !isdefined(outerfunc.fixed.mod, outerfunc.fixed.name)
        exprs_delete_methods = []
    else
        func = Core.eval(outerfunc.fixed.mod, outerfunc.fixed.name)

        exprs_delete_methods = []
        for m in methods(func)
            parameters = Base.unwrap_unionall(m.sig).parameters
            if (m.sig <: Tuple
                    && length(parameters) >= 3
                    && parameters[2] === InternalState.TraitsDisambiguationSingleton
                    && parameters[3] <: Type{<:Tuple}
                    && parameters[3] == Type{outerfunc.fixed.signature})

                push!(exprs_delete_methods, :(Base.delete_method($m)))
            end
        end
    end

    return Expr(:block, exprs_delete_methods..., expr_generic, exprs..., nothing)
end


"""
render documentation

extra effort needs to be done to properly document the outer function by referring
to innerfunctions
"""
function render(env::MacroEnv, torender::RenderDoc)
    outerfunc = torender.outer
    innerfuncs = torender.inners
    innerfunc = torender.inner

    signature = to_expr(EP.Signature_Parsed(
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
        signature_original = Markdown.parse("""
            ```julia
            $(nonfixed.expr_original.args[1])
            ```""")  # TODO this assumes that expr_original is a function, can we do this?
        push!(doc_exprs, signature_original)
        # manual doc string of respective inner function
        push!(doc_exprs, :($(WhereTraits.Utils.DocsHelpers).mygetdoc(
            $(outerfunc.fixed.mod).$(outerfunc.fixed.name),
            Tuple{WhereTraits.InternalState.TraitsDocSingleton,
                        Type{$(outerfunc.fixed.signature)},
                        Type{$(innerfunc_fixed_to_doctype(fixed))}}
        )))
        # automatic doc string of inner function definition
        expr_original = Markdown.parse("""
            Original @traits definition:
            ```julia
            $(nonfixed.expr_original)
            ```""")
        push!(doc_exprs, expr_original)
        # better visual separation
        push!(doc_exprs, separator)
    end
    # get rid of last separator
    deleteat!(doc_exprs, lastindex(doc_exprs))

    # if we are rendering code for the same module, we need to drop the module information
    # this is needed for defining the function the very first time as `MyModule.func(...) = ...` is invalid syntax
    # for the initial definition  
    name = if env.mod === outerfunc.fixed.mod
        outerfunc.fixed.name
    else
        :($(outerfunc.fixed.mod).$(outerfunc.fixed.name))
    end

    quote
        # first the documentation of the inner function as this needs to be updated BEFORE the outer doc-string
        # is updated below
        Base.@__doc__ function $name(::$(InternalState.TraitsDocSingleton),
                                                                 ::Type{$(outerfunc.fixed.signature)},
                                                                 ::Type{$(innerfunc_fixed_to_doctype(innerfunc.fixed))}) end

        # documentation of outer function (we need to manually ignore nothing docs)
        let docstring = Base.Docs.catdoc(filter(!isnothing, [$(doc_exprs...)])...)
            $(WhereTraits.Utils.DocsHelpers).@doc_signature docstring ($signature)
        end
    end
end


struct InnerFuncFixedDocSig{Args, TypeVars, WhereTraits} end
function innerfunc_fixed_to_doctype(innerfunc_fixed)
    dicts = [innerfunc_fixed.args_mapping, innerfunc_fixed.typevars_mapping, innerfunc_fixed.traits_mapping]
    types = _Dict_to_normalizedType.(dicts)
    InnerFuncFixedDocSig{types...}
end

function _Dict_to_normalizedType(d::AbstractDict)
    # TODO performance improvement possible - Symbol is called once for sorting, and once for conversion, could be combined
    ks = d |> keys |> collect |> a -> sort!(a, by=Symbol)
    rows = [Pair{Symbol(k), Symbol(d[k])} for k in ks]
    Tuple{rows...}
end

end # module
