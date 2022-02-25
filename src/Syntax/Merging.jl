module Merging
export merge_traits, merge_traitsorder

using WhereTraits: InternalState, CONFIG
using WhereTraits.Utils: MacroEnv, sortexpr
using WhereTraits.Syntax.Rendering
using WhereTraits.Syntax: Rendering

using Graphs
using Setfield
using MetaGraphs


function merge_traits(
        outerfunc_new::InternalState.DefOuterFunc,
        innerfunc_new::InternalState.DefInnerFunc;
        doc = true)

    store = InternalState.get_traitsstore(outerfunc_new.fixed.mod, outerfunc_new.fixed.name, outerfunc_new.fixed.signature)
    return merge_traits(store, outerfunc_new, innerfunc_new; doc)
end

"""
merge the new traits information into the given traitsstore
and return whatever needs to be rendered for a correct update of the traits
"""
function merge_traits(
        store_old::Union{Nothing, InternalState.TraitsStore},
        outerfunc_new::InternalState.DefOuterFunc,
        innerfunc_new::InternalState.DefInnerFunc;
        doc = true)
    
    to_be_rendered = RenderType[]

    if isnothing(store_old)
        innerfuncs_merged = InternalState.DefInnerFuncs(innerfunc_new.fixed => innerfunc_new.nonfixed)
        store_merged = InternalState.TraitsStore(
            outerfunc = outerfunc_new,
            innerfuncs = innerfuncs_merged,
            disambiguation = InternalState.DefDisambiguation(outerfunc_new.nonfixed.traits, ordered=false),
        )
        append!(to_be_rendered, [
            RenderOuterFunc(outerfunc_new),
            RenderInnerFuncs(outerfunc_new, innerfuncs_merged),
            RenderDisambiguation(outerfunc_new, innerfuncs_merged, store_merged.disambiguation)
        ])

    else
        outerfunc_old, innerfuncs = store_old.outerfunc, store_old.innerfuncs

        innerfuncs_merged = copy(innerfuncs)
        innerfuncs_merged[innerfunc_new.fixed] = innerfunc_new.nonfixed

        traits_merged = sortexpr(unique([outerfunc_old.nonfixed.traits; outerfunc_new.nonfixed.traits]))

        if traits_merged == outerfunc_old.nonfixed.traits  # if same WhereTraits, only the inner function needs to be rendered
            store_merged = @set store_old.innerfuncs = innerfuncs_merged
            append!(to_be_rendered, [
                RenderInnerFunc(outerfunc_old, innerfunc_new),
                RenderDisambiguation(outerfunc_old, innerfuncs_merged, store_old.disambiguation)
            ])
        else
            
            # outerfunc_new
            # .............

            outerfunc_merged = InternalState.DefOuterFunc(
                # outerfunc.fixed == outerfunc_old.fixed, because of same signature
                fixed = outerfunc_old.fixed,
                nonfixed = InternalState.DefOuterFuncNonFixedPart(
                    # we aggregate all unique traits and ensure order
                    traits = traits_merged,
                    innerargs_traits_mapping = merge(
                        outerfunc_old.nonfixed.innerargs_traits_mapping,
                        outerfunc_new.nonfixed.innerargs_traits_mapping,
                    ),
                ),
            )

            # disambiguation
            # ..............
    
            traits_order_merged = copy(store_old.disambiguation.traits_order)
            for trait ∈ setdiff(traits_merged, outerfunc_old.nonfixed.traits)
                if !haskey(traits_order_merged, trait, :trait)
                    @assert add_vertex!(traits_order_merged) "ERROR. couldn't add new vertex to given traits_order"
                    set_prop!(traits_order_merged, nv(traits_order_merged), :trait, trait)
                end
                # no cyclic check needed, also no transitiveclosure, as no new edges have been added, only vertices
            end
            disambiguation_merged = InternalState.DefDisambiguation(traits_order_merged)


            store_merged = InternalState.TraitsStore(
                outerfunc = outerfunc_merged,
                innerfuncs = innerfuncs_merged,
                disambiguation = disambiguation_merged,
            )

            append!(to_be_rendered, [
                RenderOuterFunc(outerfunc_merged),
                RenderInnerFuncs(outerfunc_merged, innerfuncs_merged),
                RenderDisambiguation(outerfunc_merged, innerfuncs_merged, store_merged.disambiguation)
            ])
        end
    end

    # also return all the information about the state after this merge within an update variable
    if doc && CONFIG.auto_documentation
        push!(to_be_rendered, RenderDoc(store_merged.outerfunc, store_merged.innerfuncs, innerfunc_new))
    end
    push!(to_be_rendered, RenderTraitsStore(store_merged))

    return (; store_merged, to_be_rendered)
end



function merge_traitsorder(
        outerfunc_fixed_new::InternalState.DefOuterFuncFixedPart,
        disambiguation_new::InternalState.DefDisambiguation)

    store_old = InternalState.get_traitsstore(outerfunc_fixed_new.mod, outerfunc_fixed_new.name, outerfunc_fixed_new.signature)
    return merge_traitsorder(store_old, outerfunc_fixed_new, disambiguation_new)
end

function merge_traitsorder(
        store_old::Union{Nothing, InternalState.TraitsStore},
        outerfunc_fixed_new::InternalState.DefOuterFuncFixedPart,
        disambiguation_new::InternalState.DefDisambiguation)

    if isnothing(store_old)
        outerfunc_merged = InternalState.DefOuterFunc(
            fixed = outerfunc_fixed_new,
            nonfixed = InternalState.DefOuterFuncNonFixedPart()
        )
        store_merged = InternalState.TraitsStore(
            outerfunc = outerfunc_merged,
            innerfuncs = InternalState.DefInnerFuncs(),
            disambiguation = disambiguation_new,
        )
    else
        disambiguation_merged = merge(store_old.disambiguation, disambiguation_new)
        store_merged = @set store_old.disambiguation = disambiguation_merged
    end

    to_be_rendered = RenderType[RenderTraitsStore(store_merged), RenderDisambiguation(store_merged)]
    return (; store_merged, to_be_rendered)
end


function Base.merge(disambiguation1::InternalState.DefDisambiguation, disambiguation2::InternalState.DefDisambiguation)
    g_new = copy(disambiguation1.traits_order)
    g_merge = disambiguation2.traits_order

    nv(g_new) > 0 || return copy(InternalState.DefDisambiguation(copy(g_merge)))
    
    for (; src, dst) in edges(g_merge)
        src_trait = get_prop(g_merge, src, :trait)
        dst_trait = get_prop(g_merge, dst, :trait)

        # add new vertices if needed
        # ..........................

        traits_to_be_added = [
            haskey(g_new, src_trait, :trait) ? [] : src_trait;
            haskey(g_new, dst_trait, :trait) ? [] : dst_trait;
        ]
        for trait ∈ traits_to_be_added
            @assert add_vertex!(g_new) "ERROR. couldn't add vertex to graph"
            set_prop!(g_new, nv(g_new), :trait, trait)
        end

        # add new edges if needed
        # .......................

        src_new = g_new[src_trait, :trait]
        dst_new = g_new[dst_trait, :trait]

        if !has_edge(g_new, src_new, dst_new)
            add_edge!(g_new, src_new, dst_new)
        end
        for prop ∈ (:mod, :expr_original)
            if has_prop(g_merge, src, dst, prop)
                if !has_prop(g_new, src_new, dst_new, prop)
                    prop_value = get_prop(g_merge, src, dst, prop)
                    set_prop!(g_new, src_new, dst_new, prop, prop_value)
                else
                    @debug "Property $(prop) is already defined for the edge between traits $(src_trait) and $(dst_traits) was already defined. Leaving it untouched."
                end
            end
        end
    end

    if is_cyclic(g_new)
        cyclic_traits = [[get_prop(g_new, i, :trait) for i in cycle] for cycle in cycle_basis(g)]
        error("Found the following cycles in the given traits_order. Please fix them: $(cyclic_traits)")
    end

    transitiveclosure!(g_new)

    InternalState.DefDisambiguation(g_new)
end

end  # module