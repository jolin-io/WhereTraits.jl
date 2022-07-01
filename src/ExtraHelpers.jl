module ExtraHelpers
export @traits_test, @traits_store, @traits_show_implementation

import WhereTraits
using WhereTraits: CONFIG
using WhereTraits.Utils
using WhereTraits.Syntax.Rendering
using WhereTraits.InternalState: get_traitsstores
import WhereTraits.Syntax.Rendering: render, RenderOuterFunc, RenderInnerFunc, RenderDisambiguation
using Markdown

"""
like @traits, and works within Test.@testset, but cannot be doc-stringed

needed because of https://github.com/JuliaLang/julia/issues/34263
"""
macro traits_test(expr)
  expr_traits = WhereTraits.Syntax.traits_impl(@MacroEnv, expr)
  if isa(expr_traits, Expr) && expr_traits.head == :escape
    # we need to unwrap the escape
    expr_traits = expr_traits.args[1]
  end
  # :(eval($(QuoteNode(...))) is a workaround for @testset, see https://github.com/JuliaLang/julia/issues/34263
  expr_traits = :(eval($(QuoteNode(expr_traits))))
  expr_traits = esc(expr_traits)
  expr_traits
end


"""
    @traits_store functionname

Helper function which returns all the stored states which define the traitfunctions for the given function name,
one for each traits-enabled method signature.
"""
macro traits_store(funcname)
  get_traitsstores(__module__, funcname)
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
    outerfunc, innerfuncs = store.outerfunc, store.innerfuncs
    push!(exprs, Markdown.parse("Outer function for signature $(outerfunc.fixed.signature)"))
    push!(exprs, Markdown.parse("""
    ```julia
    $(render(env, RenderOuterFunc(outerfunc))))
    ```
    """))

    push!(exprs, Markdown.parse("\n- - -\n"))
    push!(exprs, Markdown.parse("Inner functions for signature $(outerfunc.fixed.signature)"))
    for (fixed, nonfixed) in innerfuncs
      innerfunc = WhereTraits.InternalState.DefInnerFunc(fixed = fixed, nonfixed = nonfixed)
      push!(exprs, Markdown.parse("""
      ```julia
      $(render(env, RenderInnerFunc(outerfunc, innerfunc)))
      ```
      """))
      push!(exprs, Markdown.parse("\n- - -\n"))
    end
    
    push!(exprs, Markdown.parse("\n- - -\n"))
    push!(exprs, Markdown.parse("Disambiguation functions for signature $(outerfunc.fixed.signature)"))
    
    exprs_disambiguation = render(env, RenderDisambiguation(outerfunc, innerfuncs, store.disambiguation))
    for expr_disambiguation in exprs_disambiguation.args
      isnothing(expr_disambiguation) && continue
      push!(exprs, Markdown.parse("""
      ```julia
      $(expr_disambiguation)
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
