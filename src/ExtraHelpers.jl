module ExtraHelpers
export @traits_test, @traits_get_state, @traits_show_implementation
import WhereTraits
using WhereTraits: CONFIG
using WhereTraits.Utils
using WhereTraits.Syntax.Rendering
using WhereTraits.InternalState
import WhereTraits.Syntax.Rendering: render, RenderOuterFunc, RenderInnerFunc
using Markdown

"""
like @traits, and works within Test.@testset, but cannot be doc-stringed

needed because of https://github.com/JuliaLang/julia/issues/34263
"""
macro traits_test(expr_original)
  expr = macroexpand(__module__, expr_original)
  expr_traits = WhereTraits.Syntax._traits(@MacroEnv, expr, expr_original)
  # :(eval($(QuoteNode(...))) is a workaround for @testset, see https://github.com/JuliaLang/julia/issues/34263
  expr_traits = :(eval($(QuoteNode(expr_traits))))
  expr_traits = esc(expr_traits)
  if CONFIG.suppress_on_traits_definitions
    expr_traits = :(@suppress $expr_traits)
  end
  expr_traits
end


"""
    @traits_get_state functionname

Helper function which returns all the stored states which define the traitfunctions for the given function name,
one for each traits-enabled method signature.
"""
macro traits_get_state(funcname)
  get_traitsstores(__module__, funcname)
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
        && parameters[2] === WhereTraits.InternalState.TraitsStoreSingleton
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



"""
    @traits_show_implementation mytraitsenabled_function

Render a whole TraitsStore. To get an easy fealing of what is going on and inspect errors.

For debugging purposes only.
"""
macro traits_show_implementation(funcname)
  traits_show_implementation(@MacroEnv, funcname)
end

function traits_show_implementation(env, funcname)
  stores = get_traitsstores(env.mod, funcname)
  traits_show_implementation_stores(env, stores)
end



"""
render a whole TraitsStore

for debugging purposes only
"""
function traits_show_implementation_stores(env::MacroEnv, stores)
  exprs = []
  for store in stores
    outerfunc, innerfuncs = store.definitions.outer, store.definitions.inners
    push!(exprs, Markdown.parse("Outer function for signature $(outerfunc.fixed.signature)"))
    push!(exprs, Markdown.parse("""
    ```
    $(render(env, store, RenderOuterFunc(outerfunc))))
    ```
    """))
    push!(exprs, Markdown.parse("\n- - -\n"))
    push!(exprs, Markdown.parse("Inner functions for signature $(outerfunc.fixed.signature)"))
    for (fixed, nonfixed) in innerfuncs
      innerfunc = WhereTraits.InternalState.DefInnerFunc(fixed = fixed, nonfixed = nonfixed)
      push!(exprs, Markdown.parse("""
      ```
      $(render(env, store, RenderInnerFunc(outerfunc, innerfunc)))
      ```
      """))
      push!(exprs, Markdown.parse("\n- - -\n"))
    end
    # nice separtor between several outer functions
    push!(exprs, Markdown.HorizontalRule())
  end
  Markdown.MD(exprs...)
end

end # module
