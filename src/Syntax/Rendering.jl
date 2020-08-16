module Rendering
export merge_and_render_update
import WhereTraits
using WhereTraits: CONFIG
using WhereTraits.Utils
using WhereTraits.InternalState

using ExprParsers
using Markdown
using Setfield

abstract type RenderType end
struct RenderOuterFunc{Signature} <: RenderType
  outer::InternalState.DefOuterFunc{Signature}
end
struct RenderOuterAndInnerFuncs{Signature} <: RenderType
  outer::InternalState.DefOuterFunc{Signature}
  inners::InternalState.InnerFuncs
end
struct RenderInnerFunc{Signature} <: RenderType
  # we need the outerfunc to construct the innerfunc, as it depends on the ordering of the traits functions
  # which are defined in the outerfunc
  outer::InternalState.DefOuterFunc{Signature}
  inner::InternalState.DefInnerFunc
end

struct RenderDoc{Signature}
  deftraitsfunction::InternalState.DefTraitsFunction{Signature}
  inner::InternalState.DefInnerFunc
end


"""
    merge_and_render_update(env::MacroEnv, outfunc, innerfunc; [doc = true])

Merges the new traits definition (given by `outerfunc` and `innerfunc`) and renders the update.

Returns a list of Base.Expr which can be evaled or returned in macro.
"""
function merge_and_render_update(
    env::MacroEnv,
    outerfunc::InternalState.DefOuterFunc,
    innerfunc::InternalState.DefInnerFunc;
    doc = true)

  store = get_traitsstore(env.mod, outerfunc.fixed.name, outerfunc.fixed.signature)
  store_new, func_rendering, doc_rendering = _merge(env, store, outerfunc, innerfunc)

  exprs = [render(env, store_new, func_rendering)]
  if doc && CONFIG.auto_documentation
    push!(exprs, render_doc(env, store_new, doc_rendering))
  end
  push!(exprs, render_store_update(env, store_new))
  exprs
end

function render_store_update(env::MacroEnv, store::InternalState.TraitsStore)
  name = store.original_function
  if env.mod === name.mod
    # if we are rendering code for the same module, we need to drop the module information
    # this is needed for defining the function the very first time as `MyModule.func(...) = ...` is invalid syntax
    # for the initial definition
    name = name.name
  end
  :(function $(to_expr(name))(::WhereTraits.InternalState.TraitsStoreSingleton, ::Type{$(store.definitions.outer.fixed.signature)})
    $store
  end)
end

"""
merge the new traits information into the given traitsstore
and return whatever needs to be rendered for a correct update of the traits
"""
function _merge(env::MacroEnv,
                store::Union{Nothing, InternalState.TraitsStore},
                outerfunc::InternalState.DefOuterFunc,
                innerfunc::InternalState.DefInnerFunc)
  signature = outerfunc.fixed.signature
  if isnothing(store)
    mod_original, funcname_original = normalize_mod_and_name(env.mod, outerfunc.fixed.name)
    innerfuncs_new = InternalState.InnerFuncs(innerfunc.fixed => innerfunc.nonfixed)
    store_new = InternalState.TraitsStore(
      original_function = InternalState.Reference(mod_original, funcname_original),
      definitions = InternalState.DefTraitsFunction(outerfunc, innerfuncs_new)
    )
    func_rendering = RenderOuterAndInnerFuncs(outerfunc, innerfuncs_new)

  else
    outerfunc_old, innerfuncs = store.definitions.outer, store.definitions.inners

    innerfuncs_new = copy(innerfuncs)
    innerfuncs_new[innerfunc.fixed] = innerfunc.nonfixed

    outerfunc_new_nonfixed = InternalState.DefOuterFuncNonFixedPart(
      # we aggregate all unique traits and ensure order
      innerargs_traits = sortexpr(unique([outerfunc_old.nonfixed.innerargs_traits; outerfunc.nonfixed.innerargs_traits])),
    )

    if outerfunc_old.nonfixed == outerfunc_new_nonfixed  # if same WhereTraits, only the inner function needs to be rendered
      store_new = @set store.definitions = InternalState.DefTraitsFunction(outerfunc_old, innerfuncs_new)
      func_rendering = RenderInnerFunc(outerfunc_old, innerfunc)
    else
      outerfunc_new = InternalState.DefOuterFunc(
        # outerfunc.fixed == outerfunc_old.fixed, because of same signature
        fixed = outerfunc_old.fixed,
        nonfixed = outerfunc_new_nonfixed,
      )
      store_new = @set store.definitions = InternalState.DefTraitsFunction(outerfunc_new, innerfuncs_new)
      func_rendering = RenderOuterAndInnerFuncs(outerfunc_new, innerfuncs_new)
    end
  end

  # also return all the information about the state after this merge within an update variable
  doc_rendering = RenderDoc(store_new.definitions, innerfunc)
  store_new, func_rendering, doc_rendering
end


# Render
# ======

# we use special Singletons as separators to distinguish different kinds of parameters
struct _BetweenTypeVarsAndTraits end
struct _BetweenArgsAndTypeVars end


"""
render a whole TraitsStore

for debugging purposes only
"""
function render(env::MacroEnv, store::InternalState.TraitsStore)
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
function render(env::MacroEnv, store::InternalState.TraitsStore, torender::RenderOuterAndInnerFuncs)
  outerfunc, innerfuncs = torender.outer, torender.inners
  exprs = []
  push!(exprs, render(env, store, RenderOuterFunc(outerfunc)))
  for (fixed, nonfixed) in innerfuncs
    innerfunc = InternalState.DefInnerFunc(fixed = fixed, nonfixed = nonfixed)
    push!(exprs, render(env, store, RenderInnerFunc(outerfunc, innerfunc)))
  end
  flatten_blocks(Expr(:block, exprs...))
end

function _map_args(new_to_old, innerargs)
  map(innerargs) do a
    # pure `_` is currently buggy, see https://github.com/JuliaLang/julia/issues/32727
    # hence we use ::Any instead
    get(new_to_old, a, Expr(:(::), Symbol("'", a, "'"), :(Any)))
  end
end

"""
render innerfunction
(this is only possible with informations from outerfunc)
"""
function render(env::MacroEnv, store::InternalState.TraitsStore, torender::RenderInnerFunc)
  outerfunc, innerfunc = torender.outer, torender.inner
  args = [
    :(::$(InternalState.TraitsDefSingleton));
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
  # this is needed for defining the function the very first time as `MyModule.func(...) = ...` is invalid syntax
  # for the initial definition
  name = store.original_function
  if env.mod === name.mod
    name = name.name
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

"""
render outer function
"""
function render(env::MacroEnv, store::InternalState.TraitsStore, torender::RenderOuterFunc)
  outerfunc = torender.outer

  innerargs = [
    InternalState.traitsdefsingleton;
    # we need to dispatch on the signature so that different outerfuncs don't
    # overwrite each other's innerfunc
    outerfunc.fixed.signature;
    outerfunc.fixed.innerargs_args;
    _BetweenArgsAndTypeVars();
    outerfunc.fixed.innerargs_typevars;
    _BetweenTypeVarsAndTraits();
    outerfunc.nonfixed.innerargs_traits;
  ]
  innerfunc_call = EP.Call_Parsed(
    name = store.original_function,
    curlies = [],
    args = innerargs,
    kwargs = [:(kwargs...)],
  )
  # add LineNumberNode for debugging purposes
  body = Expr(:block, env.source, innerfunc_call)

  outerfunc_parsed = EP.Function_Parsed(
    name = outerfunc.fixed.name,
    curlies = outerfunc.fixed.curlies,
    args = outerfunc.fixed.args,
    kwargs = [:(kwargs...)],
    wheres = outerfunc.fixed.wheres,
    body = body,
  )
  to_expr(outerfunc_parsed)
end

"""
render documentation

extra effort needs to be done to properly document the outer function by referring
to innerfunctions
"""
function render_doc(env::MacroEnv, store::InternalState.TraitsStore, torender::RenderDoc)
  outerfunc = torender.deftraitsfunction.outer
  innerfuncs = torender.deftraitsfunction.inners
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
    signature_original = Markdown.parse("```julia\n$(nonfixed.expr_original.args[1])\n```")  # TODO this assumes that expr_original is a function, can we do this?
    push!(doc_exprs, signature_original)
    # manual doc string of respective inner function
    push!(doc_exprs, :(WhereTraits.Utils.DocsHelper.mygetdoc(
      $(to_expr(store.original_function)),
      Tuple{WhereTraits.InternalState.TraitsDocSingleton,
            Type{$(outerfunc.fixed.signature)},
            Type{$(innerfunc_fixed_to_doctype(fixed))}}
    )))
    # automatic doc string of inner function definition
    expr_original = Markdown.parse("Original @traits definition:\n```julia\n$(nonfixed.expr_original)\n```")
    push!(doc_exprs, expr_original)
    # better visual separation
    push!(doc_exprs, separator)
  end
  # get rid of last separator
  deleteat!(doc_exprs, lastindex(doc_exprs))

  # if we are rendering code for the same module, we need to drop the module information
  # this is needed for defining the function the very first time as `MyModule.func(...) = ...` is invalid syntax
  # for the initial definition
  name = store.original_function
  if env.mod === name.mod
    name = name.name
  end
  name = to_expr(name)

  quote
    # first the documentation of the inner function as this needs to be updated BEFORE the outer doc-string
    # is updated below
    Base.@__doc__ function $name(::$(InternalState.TraitsDocSingleton),
                                 ::Type{$(outerfunc.fixed.signature)},
                                 ::Type{$(innerfunc_fixed_to_doctype(innerfunc.fixed))}) end

    # documentation of outer function (we need to manually ignore nothing docs)
    let docstring = Base.Docs.catdoc(filter(!isnothing, [$(doc_exprs...)])...)
      WhereTraits.Utils.DocsHelper.@doc_signature docstring ($signature)
    end
  end
end


struct InnerFuncFixedDocSig{Args, TypeVars, WhereTraits} end
function innerfunc_fixed_to_doctype(innerfunc_fixed)
  dicts = [innerfunc_fixed.args_mapping, innerfunc_fixed.typevars_mapping, innerfunc_fixed.traits_mapping]
  types = Dict_to_normalizedType.(dicts)
  InnerFuncFixedDocSig{types...}
end

end # module
