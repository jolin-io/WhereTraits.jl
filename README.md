# Traits.jl

Welcome to `Traits.jl`. This package exports one powerful macro `@traits` with which you can extend Julia's
where syntax. Concretely the following are supported:
- dispatch on functions returning Bool
        ```julia
        @traits f(a) where {isodd(a)} = (a+1)/2
        @traits f(a) where {!isodd(a)} = a/2
        f(4) # 2.0
        f(5) # 3.0
        ```
- dispatch on functions returning Type
        ```julia
        @traits g(a) where {Base.IteratorSize(a)::Base.HasShape} = 43
        @traits g(a) = 1
        g([1,2,3]) # 43
        g(Iterators.repeated(1)) # 1
        ```
- dispatch on bounds on functions returning Types
        ```julia
        @traits h(a) where {eltype(a) <: Number} = true
        @traits h(a) = false
        h([1.0]) # true
        h([""]) # false
        ```

And all this works with arbitrary many where expressions and creates optimal code where possible via standard Julia compiler.

The package supports advanced auto-documentation which gives you a good overview about what is defined in a `@traits` function.
```julia
help?> h

  h(a1::T1; kwargs...) where T1

  ––– Original @traits definitions follow –––

  h(a) where eltype(a) <: Number

  Original @traits definition:

  function h(a) where eltype(a) <: Number
      #= none:1 =#
      true
  end

    •      •      •  

  h(a)

  Original @traits definition:

  function h(a)
      #= none:1 =#
      false
  end
```

Last but not least the macro is implemented to support proper pre-compilation as normal.

## Installation & Import

```julia
using Pkg
pkg"registry add https://github.com/JuliaRegistries/General"  # central julia repository
pkg"registry add https://github.com/schlichtanders/SchlichtandersJuliaRegistry.jl"  # custom repository
pkg"add Traits"
```

Then use this package by loading
```julia
using Traits
```
which makes one macro available `@traits`

## Implementation Details

The implementations uses only code-rewrite, creating two nested functions out of the one ``@traits`` function.
The outer function dispatches as normal, the inner function dispatches on the added traits functionality.

To inspect what is going on it is helpful to turn off the auto_documentation feature by setting
```julia
Traits.CONFIG.auto_documentation = false
```
After this the macroexpand is simpler to understand
```julia
@macroexpand @traits foo(a) where {isodd(a)} = (a+1)/2
```
which gives the following code
```julia
function var"'__traits__.Main.foo'"()
  #= /Users/s.sahm/.julia/dev/Traits/src/Syntax/Syntax.jl:291 =#
  Traits.Syntax.TraitsStore(Traits.Syntax.Reference(Main, true, Symbol("'__traits__.Main.foo'")), Traits.Utils.TypeDict{Tuple{Any,Dict{Any,Any}}}(Tuple{Type,Tuple{Any,Dict{Any,Any}}}[(Tuple{Traits.Syntax._BetweenCurliesAndArgs,Any}, ((fixed = (signature = Tuple{Traits.Syntax._BetweenCurliesAndArgs,Any}, name = :foo, curlies = Any[], args = ASTParser.Parsers.Arg_Parsed[ASTParser.Parsers.Arg_Parsed(:a1, :T1, ASTParser.Parsers.NoDefault())], wheres = Any[ASTParser.Parsers.TypeRange_Parsed(Union{}, :T1, Any)], innerargs_args = Symbol[:a1], innerargs_typevars = Symbol[:T1]), nonfixed = (innerargs_traits = Expr[:(Val{isodd(a1)}())],)), Dict((args_mapping = Dict(:a1 => :a), typevars_mapping = Dict{Symbol,Symbol}(), traits_mapping = Dict(:(Val{isodd(a1)}()) => :(var"'Val{isodd(a1)}()'"::Val{true}))) => (kwargs = Any[], body = quote
    #= none:1 =#
    (a + 1) / 2
  end, expr_original = :(function foo(a) where isodd(a)
    #= none:1 =#
    (a + 1) / 2
  end)))))]))
end

function foo(a1::T1; kwargs...) where T1
  #= none:1 =#
  (Main).var"'__traits__.Main.foo'"(Tuple{Traits.Syntax._BetweenCurliesAndArgs,Any}, a1, Traits.Syntax._BetweenArgsAndTypeVars(), T1, Traits.Syntax._BetweenTypeVarsAndTraits(), Val{isodd(a1)}(); kwargs...)
end

function var"'__traits__.Main.foo'"(::Type{Tuple{Traits.Syntax._BetweenCurliesAndArgs,Any}}, a, ::Traits.Syntax._BetweenArgsAndTypeVars, var"'T1'"::Any, ::Traits.Syntax._BetweenTypeVarsAndTraits, var"'Val{isodd(a1)}()'"::Val{true})
  #= none:1 =#
  (a + 1) / 2
end
nothing
```
It is actually easy to understand on a high level:
1. The first complex looking function `function var"'__traits__.Main.foo'"()` defines a hidden state which is needed to correctly construct the outer and inner functions required to realize the extended dispatch of `@traits`

2. The second function `function foo(a1::T1; kwargs...) where T1` is just the outer function which corresponds to normal standard user dispatch.

  It extracts extra information according to the extended where-syntax and passes it on to the inner function, which is defined last.

3. The third function is this inner function `function var"'__traits__.Main.foo'"(::Type{Tuple{Traits.Syntax._BetweenCurliesAndArgs,Any}}, a, ::Traits.Syntax._BetweenArgsAndTypeVars, var"'T1'"::Any, ::Traits.Syntax._BetweenTypeVarsAndTraits, var"'Val{isodd(a1)}()'"::Val{true})`.

  It looks a bit more complex again, let me explain the single arguments:
  1. `::Type{Tuple{Traits.Syntax._BetweenCurliesAndArgs,Any}}` dispatches on the signature of the outer function, adding support for types with Type-parameters which is why you see this extra type `Traits.Syntax._BetweenCurliesAndArgs`. If you would have dispatch for say `function MyType{Int, Bool}(a::Any, b::Any)` this would look like `::Type{Tuple{Int, Bool, Traits.Syntax._BetweenCurliesAndArgs,Any, Any}}` respectively
  2. `a` is just the standard argument
  3. after this other arguments would follow
  4. `::Traits.Syntax._BetweenArgsAndTypeVars` is again a helper for dispatch separation
  5. `var"'T1'"::Any` corresponds to the normal standard TypeParameter

    It was renamed into `var"'T1'"`, because it is actually nowhere used in the function body. If you would have used the TypeVariable `T1`, it is named plainly `T1`.
    This was implemented because the syntax actually may have to add extra typeparameters, which then for sure are not used by the code. Hence we distinguish used/unused typeparameters for better debugging/inspecting.

  6. after this other standard type parameters would follow
  7. `::Traits.Syntax._BetweenTypeVarsAndTraits` again a helper for dispatch separation
  8. `var"'Val{isodd(a1)}()'"::Val{true})` is our extended where-dispatch for Bool function

      You see that the syntax automatically wrapped the function into a `Val` and here we dispatch on `Val{true}`. The name is extra descriptive and refers to the precise function call which happens in the outer function. This is very helpful for debugging and inspecting.

If you try `@macroexpand @traits foo(a) where {!isodd(a)} = a/2` instead, you will see that it is very similar, but dispatching on `::Val{false}` instead. This is part of the special support for bool function.

Also try ``@macroexpand @traits foo(a) where {iseven(a)} = a/2`` and see what the syntax does differently.

-----------

Now execute
```julia
@traits foo(a) where {isodd(a)} = (a+1)/2
```
and repeat inspecting
`@macroexpand @traits foo(a) where {!isodd(a)} = a/2` vs ``@macroexpand @traits foo(a) where {iseven(a)} = a/2``.

Also try inspecting methods with other outerfunctions, like ``@macroexpand @traits foo(a, b) = a + b``. You will start appreciating the hidden complexity behind the `@traits` syntax.

While the syntax mapping to an outerfunction and respective innerfunctions feels very intuitive, the needed implementation is surprisingly complicated. Luckily, all this is encapsulated nicely in the `@traits` macro. Enjoy!


## Performance + Comparison with [mauro3/SimpleTraits.jl](https://github.com/mauro3/SimpleTraits.jl)

The following examples mirrors  https://github.com/mauro3/SimpleTraits.jl#performance.
We start with defining a custom function ``fn``, a custom Trait ``Tr`` and some methods for ``fn`` dispatching on the trait.
As you see, this package recommends to use julia's standard dispatch mechanism instead of custom macro definitions. This way the interactions with normal, non-trait-based dispatch becomes way more natural.
```julia
fn(x::Integer) = 1

struct Tr end
fn(x::X) where X = fn(x, traitsof(X))
fn(x::AbstractFloat, ::TypeLB(Tr)) = 2
fn(x::AbstractFloat, _) = 3

traitsof[Float32] = Tr
traitsof[Int] = Tr

fn(5) # -> 1; dispatch only happens on the type
fn(Float32(5)) # -> 2; dispatch through traits
fn(Float64(5)) # -> 3; default dispatch through traits
```
Dispatching on a different Trait does not interfere like in mauro3/SimpleTraits.jl, however works like you would expect it from julia's method dispatch.
```julia
struct Tr2 end
traitsof[Float16] = Tr2
fn(x::AbstractFloat, ::TypeLB(Tr2)) = 4

fn(Float16(5)) # -> 4; dispatch through traits
fn(Float32(5)) # -> 2; NO MethodError; nothing is overwritten, everything works like you would hope for
```
Finally the performance test: Julia is indeed inferring the very same native code.
```julia-repl
julia> @code_native fn(5)
        .text
; Function fn {
; Location: tmp.jl:1
        pushq   %rbp
        movq    %rsp, %rbp
        movl    $1, %eax
        popq    %rbp
        retq
        nopl    (%rax,%rax)
;}
julia> @code_native fn(Float16(5))
        .text
; Function fn {
; Location: tmp.jl:4
        pushq   %rbp
        movq    %rsp, %rbp
        movl    $4, %eax
        popq    %rbp
        retq
        nopl    (%rax,%rax)
;}
```
