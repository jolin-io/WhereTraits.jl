module ExtraHelpers
export @traits_test, @traits_show_implementation
import Traits
using Traits: CONFIG
using Traits.Utils
using Traits.Syntax.Rendering
using Traits.InternalState
import Traits.Syntax.Rendering: render, RenderOuterFunc, RenderInnerFunc
using Markdown

"""
like @traits, and works within Test.@testset, but cannot be doc-stringed

needed because of https://github.com/JuliaLang/julia/issues/34263
"""
macro traits_test(expr_original)
  expr = macroexpand(__module__, expr_original)
  expr_traits = Traits.Syntax._traits(@MacroEnv, expr, expr_original)
  # :(eval($(QuoteNode(...))) is a workaround for @testset, see https://github.com/JuliaLang/julia/issues/34263
  expr_traits = :(eval($(QuoteNode(expr_traits))))
  expr_traits = esc(expr_traits)
  if CONFIG.suppress_on_traits_definitions
    expr_traits = :(@suppress $expr_traits)
  end
  expr_traits
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
  store = getorcreate_traitsstore(env.mod, funcname)
  traits_show_implementation(env, store)
end

"""
render a whole TraitsStore

for debugging purposes only
"""
function traits_show_implementation(env::MacroEnv, store::Traits.InternalState.TraitsStore)
  exprs = []
  for deftraitsfunction in values(store)
    outerfunc, innerfuncs = deftraitsfunction.outer, deftraitsfunction.inners
    push!(exprs, Markdown.parse("Outer function for signature $(outerfunc.fixed.signature)"))
    push!(exprs, Markdown.parse("""
    ```
    $(render(env, store, RenderOuterFunc(outerfunc))))
    ```
    """))
    push!(exprs, Markdown.parse("\n- - -\n"))
    push!(exprs, Markdown.parse("Inner functions for signature $(outerfunc.fixed.signature)"))
    for (fixed, nonfixed) in innerfuncs
      innerfunc = Traits.InternalState.DefInnerFunc(fixed = fixed, nonfixed = nonfixed)
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
