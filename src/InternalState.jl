# Internal State of the syntax
# ============================

module InternalState
export get_traitsstore, get_traitsstores, TraitsStore
export WhereTraitsException, WhereTraitsAmbiguityError

using WhereTraits: InternalState
using WhereTraits.Utils: normalize_mod_and_name
using StructEquality
using ExprParsers
using ProxyInterfaces

using Graphs
using MetaGraphs

@struct_hash_equal Base.@kwdef struct DefOuterFuncFixedPart{Signature}
    # everything under fixed should be identifiable via signature
    signature::Type{Signature}
    mod::Module
    name::Symbol
    curlies::Vector{Union{Symbol, Expr}}
    args::Vector{EP.Arg_Parsed}
    wheres::Vector{EP.TypeRange_Parsed}
    # body information, also fixed by signature, normalized naming
    innerargs_args::Vector{Symbol}
    innerargs_typevars::Vector{Symbol}
end

@struct_hash_equal Base.@kwdef struct DefOuterFuncNonFixedPart
    traits::Vector{Union{Symbol, Expr}} = []
    innerargs_traits_mapping::Dict{Union{Symbol, Expr}, Union{Symbol, Expr}} = Dict{Union{Symbol, Expr}, Union{Symbol, Expr}}()
end

@struct_hash_equal Base.@kwdef struct DefOuterFunc{Signature}
    fixed::DefOuterFuncFixedPart{Signature}
    nonfixed::DefOuterFuncNonFixedPart
end

@struct_hash_equal Base.@kwdef struct DefInnerFuncFixedPart
    args_mapping::Dict{Symbol, Symbol}
    typevars_mapping::Dict{Symbol, Symbol}
    traits_mapping::Dict{Union{Symbol, Expr}, Union{Symbol, Expr}}
end

@struct_hash_equal Base.@kwdef struct DefInnerFuncNonFixedPart
    mod::Module
    kwargs::Vector{Expr}
    body::Expr
    expr_original::Expr
end

@struct_hash_equal Base.@kwdef struct DefInnerFunc
    fixed::DefInnerFuncFixedPart
    nonfixed::DefInnerFuncNonFixedPart
end

@struct_hash_equal Base.@kwdef struct DefDisambiguation
    """ you can use a traitsname::Symbol to index into traits_order like `traits_order[traitname, :traits]`"""
    traits_order::MetaDiGraph
end

"""
    DefDisambiguation([:(trait1(a1)), :(trait2(a1, a2))], ordered=true, mod=nothing, expr_original=nothing)

creates a DefDisambiguation with
- given traits as nodes
- the trait as the `:trait` property of the respective node
- if `ordered=true`, then also directed edges are added from the last to the secondlast, from the secondlast to the thridlast, ..., to the first 
- `mod` and `expr_original` are added as further meta data to these edges

further ensures that the DiGraph is acyclic and has the transitiveclosure
"""
function DefDisambiguation(traits::Vector{<:Union{Symbol, Expr}}; ordered = true, mod = nothing, expr_original = nothing)
    indices_unique = unique(i -> traits[i], eachindex(traits))
    @assert length(indices_unique) == length(traits) "Found duplicates in the given traits: $(traits[setdiff(eachindex(traits), indices_unique)])"

    g = MetaDiGraph(DiGraph(length(traits)))
    # add traits as names of vertices
    for (i, trait) in enumerate(traits)
        set_prop!(g, i, :trait, trait)
    end
    set_indexing_prop!(g, :trait)

    if ordered
        for i in 1:(length(traits) - 1)
            # an edge is interpreted as ≤, and the first one in the list is highest
            add_edge!(g, i+1, i)
            isnothing(mod) || set_prop!(g, i, i+1, :mod, mod)
            isnothing(expr_original) || set_prop!(g, i, i+1, :expr_original, expr_original)
        end

        if is_cyclic(g)
            cyclic_traits = [traits[cycle] for cycle in cycle_basis(g)]
            error("Found the following cycles in the given traits_order. Please fix them: $(cyclic_traits)")
        end

        transitiveclosure!(g)
    end

    return DefDisambiguation(g)
end


const DefInnerFuncs = Dict{DefInnerFuncFixedPart, DefInnerFuncNonFixedPart}


@struct_hash_equal Base.@kwdef struct TraitsStore{Signature}
    outerfunc::DefOuterFunc{Signature}
    innerfuncs::DefInnerFuncs
    disambiguation::DefDisambiguation
end

"""
returns TraitsStore or nothing if no TraitsStore could be found
"""
function get_traitsstore(mod, funcname, signature)
    mod_original, funcname_original = normalize_mod_and_name(mod, funcname)
    try
        # call with special `traitsstore` argument to get the store
        return getproperty(mod_original, funcname_original)(traitsstoresingleton, signature)
    catch e
        e isa Union{UndefVarError, MethodError} || rethrow()
        return nothing
    end
end

function get_traitsstores(mod, funcname)
    mod_original, funcname_original = normalize_mod_and_name(mod, funcname)
    isdefined(mod_original, funcname_original) || return []

    func = Core.eval(mod_original, funcname_original)
    traits_enabled_signatures = []
    for m in methods(func)
        parameters = Base.unwrap_unionall(m.sig).parameters
        if (m.sig <: Tuple
                && length(parameters) == 3
                && parameters[2] === InternalState.TraitsStoreSingleton
                && parameters[3] <: Type{<:Tuple})
            # third parameter is the Type{SignatureTuple}
            signature = parameters[3].parameters[1]
            push!(traits_enabled_signatures, signature)
        end
    end
    for sig in traits_enabled_signatures
        if isnothing(get_traitsstore(mod, funcname, sig))
            error("mod = $mod, funcname = $funcname, sig = $sig")
        end
    end
    [get_traitsstore(mod, funcname, sig) for sig in traits_enabled_signatures]
end


abstract type TraitsSingleton end

"""
        traitsdefsingleton

Used to mark a function method as belonging to the traitsdefinition
"""
struct TraitsDefSingleton <: TraitsSingleton end
const traitsdefsingleton = TraitsDefSingleton()

"""
    traitsdisambiguationsingleton

Used to mark a function method as belonging to the disambiguation pre-phase, checking for ambiguation errors
"""
struct TraitsDisambiguationSingleton <: TraitsSingleton end
const traitsdisambiguationsingleton = TraitsDisambiguationSingleton()

"""
        traitsstoresingleton

Used to mark a function method as belonging to the traitsstore
"""
struct TraitsStoreSingleton <: TraitsSingleton end
const traitsstoresingleton = TraitsStoreSingleton()

"""
        traitsdocsingleton

Used to mark a function method as used for the auto documentation feature
"""
struct TraitsDocSingleton <: TraitsSingleton end
const traitsdocsingleton = TraitsDocSingleton()



abstract type WhereTraitsException <: Exception end
struct WhereTraitsAmbiguityError <: WhereTraitsException
    traits_conflicting::Vector{Union{Symbol, Expr}}
    functioncall_signature::Expr
end

function Base.showerror(io::IO, e::WhereTraitsAmbiguityError)
    body = Expr(:block, e.traits_conflicting...)
                
    macro_call = Expr(:macrocall, Symbol("@traits_order"), nothing, e.functioncall_signature, body) 
    expr_string = string(macro_call)

    print(io,"""
    Disambiguity found. Please specify an ordering between traits, like the following.

        $expr_string
    """)
end

end # module
