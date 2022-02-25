module Ambiguities
export ambiguity, AbstractAmbiguity, Ambiguity, NoAmbiguity
export resolve, NoResolution, Resolution

using Pipe: @pipe
using SimpleMatch: @match
using IterTools: subsets
using WhereTraits.Utils: Counter
using StructEquality
using Base: @kwdef
using Graphs

"""
No support for
- lowerbounds
- upperbounds Union types (possible to add in the future via flattening out into multiple definitions)
"""


abstract type AbstractAmbiguity end

struct NoAmbiguity <: AbstractAmbiguity end

@struct_hash_equal @kwdef struct Ambiguity <: AbstractAmbiguity
    indices::Vector{Int}
    mostspecific::Vector{Vector{Int64}}  # when comparing multiple dispatches there might be non-unique most-specific
    dispatch_resolution::Vector{Type}  # only upperbounds
end

function Base.isless(a::Ambiguity, b::Ambiguity)
    if length(a.indices) == length(b.indices)
        a.indices < b.indices
    else
        length(a.indices) < length(b.indices)
    end
end


"""
    ambiguity([Any, Number, ...], [Integer, Int, ...]) -> AbstractAmbiguity

Checks whether two dispatches on the same type-hierarchies might be ambiguous.

Type-hierarchies are identified by position in the vector.
Each element of a dispatch is the respective `UpperBound` of a dispatch like `Type{<:UpperBound}`.
"""
function ambiguity(dispatch1, dispatch2; i1=1, i2=2)
    # Nothing for no one more specific, Int for first or second
    indices = sort(union(i1, i2))
    mostspecific = fieldtype(Ambiguity, :mostspecific)(undef, length(dispatch1))
    fill!(mostspecific, indices)
    dispatch_resolution = fieldtype(Ambiguity, :dispatch_resolution)(undef, length(dispatch1))
    fill!(dispatch_resolution, Any)
    count1 = 0
    count2 = 0

    for (i, ub1, ub2) in zip(Counter(), dispatch1, dispatch2)
        if ub1 != ub2
            if ub1 <: ub2
                dispatch_resolution[i] = ub1
                mostspecific[i] = sort(union(i1))
                count1 += 1
            elseif ub1 >: ub2
                dispatch_resolution[i] = ub2
                mostspecific[i] = sort(union(i2))
                count2 += 1
            else
                # ub1 and ub2 are completely separate type hierarchies, hence the dispatches cannot be ambiguous
                return NoAmbiguity()
            end
        else
            dispatch_resolution[i] = ub1  # might be smaller than Any
            # mostspecifiy is not more specific than default
            # no count
        end
    end
    if count1 == 0 || count2 == 0
        # one dispatch is just more specific than the other
        return NoAmbiguity()
    else
        return Ambiguity(
            indices,
            mostspecific,
            dispatch_resolution,
        )
    end
end

"""
    ambiguity(::Ambiguity, ::Ambiguity) -> AbstractAmbiguity

finds the ambiguity between two given ambiguities
"""
function ambiguity(ambiguity1::Ambiguity, ambiguity2::Ambiguity)
    _ambiguity_merged = ambiguity(ambiguity1.dispatch_resolution, ambiguity2.dispatch_resolution)
    
    if isa(_ambiguity_merged, NoAmbiguity)
        return NoAmbiguity()
    end
    
    mostspecific = fieldtype(Ambiguity, :mostspecific)(undef, length(ambiguity1.mostspecific))
    for (i, smerged, s1, s2) in zip(Counter(), _ambiguity_merged.mostspecific, ambiguity1.mostspecific, ambiguity2.mostspecific)
        soriginal = [s1, s2]
        mostspecific[i] = sort(union(soriginal[smerged]...))
    end
    
    ambiguity_merged = Ambiguity(
        sort(union(ambiguity1.indices, ambiguity2.indices)),
        mostspecific,
        _ambiguity_merged.dispatch_resolution,
    )

    # only return if we found something new
    if ambiguity_merged ∈ (ambiguity1, ambiguity2)
        return NoAmbiguity()
    else
        return ambiguity_merged
    end
end

function Base.merge(ambiguity1::Ambiguity, ambiguity2::Ambiguity)
    @assert ambiguity1.dispatch_resolution == ambiguity2.dispatch_resolution """
        `Base.merge` for Ambiguities makes only sense if the `dispatch_resolution` are the same. Please use `Ambiguities.ambiguity` instead."
        """
    indices = sort(union(ambiguity1.indices, ambiguity2.indices))
    mostspecific = map((a, b) -> sort(union(a, b)), ambiguity1.mostspecific, ambiguity2.mostspecific)
    return Ambiguity(
        indices,
        mostspecific,
        ambiguity1.dispatch_resolution
    )
end

"""
    ambiguities([[Any, Number, ...], [Integer, Int, ...], ...]) -> Set{Ambiguity}

given a number dispatches, it returns the found ambiguities between these dispatches which would lead to MethodErrors if not fixed otherwise
"""
function ambiguities(dispatches)::Set{Ambiguity}
    
    # ambiguities between 2
    # -------------------
    ambiguities_pair = @pipe subsets(eachindex(dispatches), 2) |>
        Iterators.map(_) do (a, b)
            ambiguity(dispatches[a], dispatches[b], i1=a, i2=b)
        end |>
        Iterators.filter(_) do ambiguity
            isa(ambiguity, Ambiguity) && ambiguity.dispatch_resolution ∉ dispatches
        end |>
        collect

    # conflicts between >2
    # --------------------
    ambiguities_all = Set(ambiguities_pair)

    _ambiguities_combined = Set{Ambiguity}()
    _ambiguities_single = ambiguities_all
    while !isempty(_ambiguities_single)
        for (ambiguity1, ambiguity2) in subsets(collect(_ambiguities_single), 2)
            # if the ambiguities don't overlap, nothing is to be combined
            isdisjoint(ambiguity1.indices, ambiguity2.indices) && continue

            # it can happen that two ambiguities result in the same dispatch
            # we cannot realize this, as the one would overwrite the other, hence we need to merge those and delete the previous ones
            needs_merge = ambiguity1.dispatch_resolution == ambiguity2.dispatch_resolution

            ambiguity_combined = if needs_merge
                delete!(ambiguities_all, ambiguity1)
                delete!(ambiguities_all, ambiguity2)
                merge(ambiguity1, ambiguity2)
            else
                ambiguity(ambiguity1, ambiguity2)
            end

            if isa(ambiguity_combined, Ambiguity) &&
                    ambiguity_combined.dispatch_resolution ∉ dispatches &&
                    ambiguity_combined ∉ ambiguities_all &&
                    ambiguity_combined ∉ _ambiguities_combined
                    
                push!(_ambiguities_combined, ambiguity_combined)
            end
        end
        union!(ambiguities_all, _ambiguities_combined)
        _ambiguities_single = _ambiguities_combined
        _ambiguities_combined = Set{Ambiguity}()
    end

    return ambiguities_all
end



abstract type AbstractResolution end

struct Resolution <: AbstractResolution
    i_method::Int
    dispatch::Vector{Type}
end

struct NoResolution{T} <: AbstractResolution
    indices_traits_conflicting::T
end


"""
    resolve(ambiguity::Ambiguity, partial_order) -> Union{Int, Unresolvable}

resolves the given ambiguity in the sense of either returning an `Unresolvable` 
or by returning the mostspecific index which wins given the `partial_order`
"""
function resolve(ambiguity::Ambiguity, traits, partial_order)
    indices_traits_conflicting = [i
        for (i, s) ∈ enumerate(ambiguity.mostspecific)
        if s != ambiguity.indices]

    indices_partialorder = [partial_order[trait, :trait] for trait in traits]
    indices_partialorder
    
    indices_partialorder_conflicting = indices_partialorder[indices_traits_conflicting]

    subgraph, _vmap_subgraph = induced_subgraph(partial_order, indices_partialorder_conflicting)
    # we can use our indices_traits_conflicting directly to get to the indices belonging to `traits`
    vmap_subgraph = indices_traits_conflicting

    # the difficulty is to deal with a partial ordering
    # there may be subcomponents
    # for each such we check the elements which dominate
    # there may be several per subcomponent
    # we collect all for each site and hope that one site gathers all attractors
    indices_traits_attracting = Int[]
    for component in weakly_connected_components(subgraph)
        component_graph, vmap_component = induced_subgraph(subgraph, component)
        for attracting_component in attracting_components(component_graph)
            index_trait_attracting = vmap_subgraph[vmap_component[only(attracting_component)]]
            push!(indices_traits_attracting, index_trait_attracting)
        end
    end

    indices_dispatches_attracting = sort(union(ambiguity.mostspecific[indices_traits_attracting]...))

    if length(indices_dispatches_attracting) == length(ambiguity.indices)
        # the partial_order was not able to give us any information at all 
        return NoResolution(indices_traits_attracting)
    
    elseif length(indices_dispatches_attracting) > 1
        submostspecific = map(ambiguity.mostspecific) do s
            intersect(s, indices_dispatches_attracting)
        end
        subambiguity = Ambiguity(
            indices_dispatches_attracting,
            submostspecific,
            ambiguity.dispatch_resolution,
        )
        resolve(subambiguity, traits, partial_order)

    elseif length(indices_dispatches_attracting) == 0
        error("This should never happen, found an attracting trait without a possible value")

    else  # length == 1
        return Resolution(
            only(indices_dispatches_attracting),
            ambiguity.dispatch_resolution,
        )
    end 
end

end  # module