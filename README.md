<h1> Traits.jl </h1>

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

*Warning: While the dispatch works for dynamic functions, it will only be able to create optimal code if your traits function supports proper type-inference. E.g. you can use `Base.isempty`, however type-inference cannot see whether it will return true or false by static inspection. Hence it will use slower dynamic code.*




**Table of Contents**
<!-- TOC START min:1 max:3 link:true asterisk:true update:true -->
  * [Installation & Import](#installation--import)
  * [Implementation Details](#implementation-details)
  * [Performance + Comparison with mauro3/SimpleTraits.jl](#performance--comparison-with-mauro3simpletraitsjl)
  * [Traits.BasicTraits](#traitsbasictraits)
  * [Dispatch on whether functions are defined - using IsDef.jl](#dispatch-on-whether-functions-are-defined---using-isdefjl)
  * [Current Restrictions and Future Plans](#current-restrictions-and-future-plans)
<!-- TOC END -->



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

For a high-level comparison between ``Traits.@traits`` and ``SimpleTraits.jl`` see the respective [discourse discussion](https://discourse.julialang.org/t/announcing-traits-jl-a-revival-of-julia-traits/35683/5?u=schlichtanders).

The following examples mirror https://github.com/mauro3/SimpleTraits.jl#details-of-method-dispatch.
We start with defining a custom function ``fn``, a custom Trait function ``isTr`` and some methods for ``fn`` dispatching on the trait.
```julia
isTr(_) = false

fn(x::Integer) = 1
@traits fn(x::X) where {X<:AbstractFloat, isTr(X)} = 2
@traits fn(x::AbstractFloat) = 3

fn(Float32(5)) # 3; dispatch through traits, but isTr not yet defined, hence using default case

isTr(::Type{Float32}) = true
isTr(::Type{Int}) = true

fn(5) # 1; dispatch only happens on the type
fn(Float32(5)) # 2; dispatch through traits
fn(Float64(5)) # 3; default dispatch through traits


isTr2(_) = false
isTr2(::Type{Float16}) = true
@traits fn(x::X) where {X <: AbstractFloat, isTr2(X)} = 4

fn(Float16(5)) # 4; dispatch through traits
fn(Float32(5)) # 2; NO MethodError; nothing is overwritten, everything works like you would hope for
```

Finally the performance test: Julia is indeed inferring the very same native code.
```julia-repl
julia> @code_native fn(5)
    .section    __TEXT,__text,regular,pure_instructions
; ┌ @ none:1 within `fn'
    movl    $1, %eax
    retq
    nopw    %cs:(%rax,%rax)
; └
julia> @code_native fn(Float16(5))
    .section    __TEXT,__text,regular,pure_instructions
; ┌ @ none:1 within `fn'
    movl    $4, %eax
    retq
    nopw    %cs:(%rax,%rax)
; └
```

## Traits.BasicTraits

For anology with SimpleTraits.jl, this package comes with standard traits definitions
``ismutable``, ``isimmutable``, `isiterable`, `iscallable`, `isbitstype`, `isconcretetype`. They mostly just wrap respective standard definitions in ``Base``, with the added benefit, that they behave similarly to `Base.eltype` in that they have the convenience fallback `ismutable(value) = ismutable(typeof(value))`.

You can use these by executing the following
```julia
using Traits
using Traits.BasicTraits
Traits.BasicTraits.@overwrite_Base

ismutable(1)  # false
ismutable("string")  # true
ismutable(String)  # true
```

Please consult the file [test/BasicTraits.jl](https://github.com/schlichtanders/Traits.jl/blob/master/test/BasicTraits.jl) for more examples.


## Dispatch on whether functions are defined - using [IsDef.jl](https://github.com/schlichtanders/IsDef.jl)

You want to dispatch on whether a function is defined or not? I guess this is a standard scenario and hence I tried to support it, and extracted it into another package.

[IsDef.jl](https://github.com/schlichtanders/IsDef.jl) exports two functions `isdef` and `Out` with which you can dispatch on whether functions are defined or not.
([IsDef.jl](https://github.com/schlichtanders/IsDef.jl) is a sub-dependency of Traits.jl, so you should already have it installed).
With `IsDef.isdef`/`IsDef.Out` and ``Traits.@traits`` we can define typesafe dispatch like follows:

```julia
using Traits
using IsDef
using Test

struct MyError <: Exception
  msg::AbstractString
end
MyError() = MyError("")

@traits typesafe_call(f, a::T) where {T, isdef(f, T)} = f(a)
# note that you can also use isdef on args directly, like you can use eltype on args
# however this will be recognized as a different traitfunction compared to isdef(f, T)
# which can lead to ambiguous overloadings. Just good to keep in mind
@traits typesafe_call(f, a) where {!isdef(f, a)} = throw(MyError("given function cannot work with given arg"))
@code_llvm typesafe_call(x -> x+2, 4)  # looks really good

@test typesafe_call(x -> x+2, 4) == 6
@test_throws MyError typesafe_call(x -> x+2, "string")


@traits typesafe_out(f, a) where {Out(f, a) <: Number} = f(a) + 6
@traits typesafe_out(f, a) where {Out(f, a) <: String} = "yeah $(f(a))!"
@traits typesafe_out(f, a) = throw(MyError("no match"))

@test typesafe_out(x -> 3x, 1) == 9
@test typesafe_out(x -> "$x $x", 1) == "yeah 1 1!"
@test_throws MyError typesafe_out(x -> convert(Vector, x), 1)


# more complex case with dependencies among functions
@traits function typesafe_aggregate(a::Vector{A}, introduce, combine) where
    {A, isdef(introduce, A), isdef(combine, Out(introduce, A), Out(introduce, A))}

  reduce(combine, introduce.(a))
end
@traits typesafe_aggregate(a::Vector, introduce, combine) = throw(MyError("TypeError"))

@test typesafe_aggregate(["this","is","a","test"], length, +) == 11
@test typesafe_aggregate(["this","is","a","test"], length, *) == 32
@test typesafe_aggregate(["this","is","a","test"], x -> "$x ", *) == "this is a test "
@test_throws MyError typesafe_aggregate(["this","is","a", "test"], x->x, +)
```

This is very powerful. Be warned that `IsDef` has limitations currently because julia type-inference has limitations. Luckily the type-inference is already very good with concrete-types, which is what we need for dispatch.


## Current Restrictions and Future Plans

Currently the ``@traits`` macro has some known restrictions which are soon to be solved:
* the extended where syntax is currently implemented on **symbol level**, which is why traits functions like `Base.IteratorSize` and the non-qualified `IteratorSize` (assuming you imported `import Base:IteratorSize`) are treated as two different functions, despite being the same. So for now try to only use the one style or the other.

    The plan is to fix this by looking up method definitions in the caller module.

* currently **only top-level functions** are directly supported, as the syntax stores and needs information about previous function definitions. An alternative syntax is planned which will support `@traits` on functions within other scopes like functions in functions.

    The idea I have is to support a block syntax alternative
    which assumes that there is no further outer state to take into account, but the block stands on its own. This would still look a bit clumsy, but semantically it is probably the way to go.
```julia
function func()
  @traits begin  # not yet supported
    function nested(a) where ...
    end
    function nested(a) where ...
    end
  end
end
```
* The ``@traits`` currently does not work well within the `Test.@testset` macro. As a workaround Traits.jl exports a `@traits_test` variant which works better, but still has cases where it fails. This needs to be investigated further, and maybe needs a fix on ``Test.@testset``, don't know.
