<h1> WhereTraits.jl </h1>

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://schlichtanders.github.io/WhereTraits.jl/stable)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://schlichtanders.github.io/WhereTraits.jl/dev)
[![Build Status](https://github.com/schlichtanders/WhereTraits.jl/workflows/CI/badge.svg)](https://github.com/schlichtanders/WhereTraits.jl/actions)
[![Coverage](https://codecov.io/gh/schlichtanders/WhereTraits.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/schlichtanders/WhereTraits.jl)


Welcome to `WhereTraits.jl`. This package exports one powerful macro `@traits` with which you can extend Julia's
where syntax. Concretely the following are supported:
- dispatch on functions returning Bool
```julia
@traits f(a) where {isodd(a)} = (a+1)/2
@traits f(a) where {!isodd(a)} = a/2
f(4) # 2.0
f(5) # 3.0
```
- dispatch on functions returning anything
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

  (h(a) where eltype(a) <: Number) = begin
          #= none:1 =#
          true
      end

    •      •      •  

  h(a)

  Original @traits definition:

  h(a) = begin
          #= none:1 =#
          false
      end
```

*Warning: While the dispatch works for dynamic functions, it will only be able to create optimal code if your traits function supports proper type-inference. E.g. you can use `Base.isempty`, however type-inference cannot see whether it will return true or false by static inspection. Hence it will use slower dynamic code.*




**Table of Contents**
<!-- TOC START min:1 max:3 link:true asterisk:true update:true -->
  * [Installation & Import](#installation--import)
  * [Implementation Details](#implementation-details)
  * [Performance + Comparison with mauro3/SimpleTraits.jl](#performance--comparison-with-mauro3simpletraitsjl)
  * [WhereTraits.BasicTraits](#wheretraitsbasictraits)
  * [Dispatch on whether functions are defined - using IsDef.jl](#dispatch-on-whether-functions-are-defined---using-isdefjl)
  * [Current Restrictions and Future Plans](#current-restrictions-and-future-plans)
<!-- TOC END -->



## Installation & Import

```julia
using Pkg
pkg"registry add https://github.com/JuliaRegistries/General"  # central julia registry
pkg"registry add https://github.com/schlichtanders/SchlichtandersJuliaRegistry.jl"  # custom registry
pkg"add WhereTraits"
```

Then use this package by loading
```julia
using WhereTraits
```
which makes one macro available `@traits`

## Implementation Details

The implementations uses only code-rewrite, creating two nested functions out of the one `@traits` function.
The outer function dispatches as normal, the inner function dispatches on the added traits functionality.

To inspect what is going on it is helpful to turn off the auto_documentation feature by setting
```julia
WhereTraits.CONFIG.auto_documentation = false
```
After this the macroexpand is simpler to understand
```julia
@macroexpand @traits foo(a) where {isodd(a)} = (a+1)/2
```
which gives the following code
```julia
function foo(a1::T1; kwargs...) where T1
    #= none:1 =#
    (Main).foo(
      WhereTraits.InternalState.TraitsSingleton(),
      Tuple{WhereTraits.Syntax.Parsing._BetweenCurliesAndArgs,Any},
      a1,
      WhereTraits.Syntax.Rendering._BetweenArgsAndTypeVars(),
      T1,
      WhereTraits.Syntax.Rendering._BetweenTypeVarsAndTraits(),
      Val{isodd(a1)}();
      kwargs...)
end

function foo(::WhereTraits.InternalState.TraitsSingleton,
             ::Type{Tuple{WhereTraits.Syntax.Parsing._BetweenCurliesAndArgs,Any}},
             a,
             ::WhereTraits.Syntax.Rendering._BetweenArgsAndTypeVars,
             var"'T1'"::Any,
             ::WhereTraits.Syntax.Rendering._BetweenTypeVarsAndTraits,
             var"'Val{isodd(a1)}()'"::Val{true})
    #= none:1 =#
    (a + 1) / 2
end

function foo(::WhereTraits.InternalState.TraitsSingleton)
    #= /Users/s.sahm/.julia/dev/WhereTraits/src/Syntax/Rendering.jl:111 =#
    WhereTraits.InternalState.TraitsStore(WhereTraits.InternalState.Reference(Main, :foo), WhereTraits.Utils.TypeDict{WhereTraits.InternalState.DefTraitsFunction}(Tuple{Type,WhereTraits.InternalState.DefTraitsFunction}[(Tuple{WhereTraits.Syntax.Parsing._BetweenCurliesAndArgs,Any}, WhereTraits.InternalState.DefTraitsFunction{Tuple{WhereTraits.Syntax.Parsing._BetweenCurliesAndArgs,Any}}(WhereTraits.InternalState.DefOuterFunc{Tuple{WhereTraits.Syntax.Parsing._BetweenCurliesAndArgs,Any}}(WhereTraits.InternalState.DefOuterFuncFixedPart{Tuple{WhereTraits.Syntax.Parsing._BetweenCurliesAndArgs,Any}}(Tuple{WhereTraits.Syntax.Parsing._BetweenCurliesAndArgs,Any}, :foo, Union{Expr, Symbol}[], ExprParsers.Arg_Parsed[EP.Arg_Parsed(name=:a1, type=:T1, default=ExprParsers.NoDefault())], ExprParsers.TypeRange_Parsed[EP.TypeRange_Parsed(lb=Union{}, name=:T1, ub=Any)], Symbol[:a1], Symbol[:T1]), WhereTraits.InternalState.DefOuterFuncNonFixedPart(Union{Expr, Symbol}[:(Val{isodd(a1)}())])), Dict(WhereTraits.InternalState.DefInnerFuncFixedPart(Dict(:a1 => :a), Dict{Symbol,Symbol}(), Dict{Union{Expr, Symbol},Union{Expr, Symbol}}(:(Val{isodd(a1)}()) => :(var"'Val{isodd(a1)}()'"::Val{true}))) => WhereTraits.InternalState.DefInnerFuncNonFixedPart(Expr[], quote
#= none:1 =#
(a + 1) / 2
end, :((foo(a) where isodd(a)) = begin
      #= none:1 =#
      (a + 1) / 2
  end)))))]))
end
nothing
```
It is actually easy to understand on a high level:

1. The first function `function foo(a1::T1; kwargs...) where T1` is the so called "outer" function which does all the normal standard Julia dispatch. It is the necessary initial entry point in order to then perform a subsequent call to further dispatch on traits.

  In the function body you see that this outer function extracts extra information according to the extended where-syntax. Lets go through the arguments one by one

  1. `WhereTraits.InternalState.TraitsSingleton()` is a helper type indicating that this is a call to a traits inner function
  2. `Tuple{WhereTraits.Syntax.Parsing._BetweenCurliesAndArgs,Any}` is the complete function signature of the outer function, with an additional helper `_BetweenCurliesAndArgs` to deal with TypeParameters of UnionAll types (whereupon which you can also define function calls and hence `@traits`)
  3. `a1` is the first actual argument (after this `a2`, `a3` and etc. could follow in principle)
  4. `WhereTraits.Syntax.Rendering._BetweenArgsAndTypeVars()` is again a helper type to distinguish args from typevariables
  5. `T1` is a type parameter (again here `T2`, `T3`, etc. would follow if there are more typeparameters)
  6. `WhereTraits.Syntax.Rendering._BetweenTypeVarsAndTraits()` is another helper, now separating the traits definitions
  7. `Val{isodd(a1)}()` here comes our first actual trait definition (if you define more traits, they would follow here)
  8. `; kwargs...` at last all kwargs are just passed through (dispatch on kwargs is not yet supported)

  All these arguments are passed on to the inner function, which is defined next.

2. The second function is this inner function `function foo(::WhereTraits.InternalState.TraitsSingleton, ::Type{Tuple{WhereTraits.Syntax._BetweenCurliesAndArgs,Any}}, a, ::WhereTraits.Syntax._BetweenArgsAndTypeVars, var"'T1'"::Any, ::WhereTraits.Syntax._BetweenTypeVarsAndTraits, var"'Val{isodd(a1)}()'"::Val{true})`.

  Here we do the actual full traits dispatch, specifying a dispatch type for each of the arguments we just put into the inner functions. Let's again go through each single argument:

  1. `::WhereTraits.InternalState.TraitsSingleton` dispatches on the singleton type to make sure this does not conflict with any other methods defined for this function
  2. `::Type{Tuple{WhereTraits.Syntax._BetweenCurliesAndArgs,Any}}` dispatches on the signature of the outer function, again adding support for types with Type-parameters which is why you see this extra type `WhereTraits.Syntax._BetweenCurliesAndArgs`. If you would have dispatch for say `function MyType{Int, Bool}(a::Any, b::Any)` this would look like `::Type{Tuple{Int, Bool, WhereTraits.Syntax._BetweenCurliesAndArgs,Any, Any}}` respectively
  3. `a` is just the standard argument, which was of type `Any`.

    Hereafter, other arguments would follow.

  4. `::WhereTraits.Syntax._BetweenArgsAndTypeVars` is again a helper for dispatch separation
  5. `var"'T1'"::Any` corresponds to the normal standard TypeParameter, here of type Any.

    It was renamed into `var"'T1'"`, because it is actually nowhere used in the function body. If you would have used the TypeVariable `T1`, it is named plainly `T1`.
    This was implemented because the syntax actually may have to add extra typeparameters, which then for sure are not used by the code. Hence we distinguish used/unused typeparameters for better debugging/inspecting.

    Hereafter, other standard type parameters would follow.

  6. `::WhereTraits.Syntax._BetweenTypeVarsAndTraits` is the last helper for dispatch separation
  7. `var"'Val{isodd(a1)}()'"::Val{true})` is our extended where-dispatch for Bool function

      You see that the syntax automatically wrapped the function into a `Val` and here we dispatch on `Val{true}`. The name is extra descriptive and refers to the precise function call which happens in the outer function. This can be helpful for debugging and inspecting.

  8. This function does not define any keyword arguments.

3. The last complex looking function is `function foo(::WhereTraits.InternalState.TraitsSingleton)`. It again uses the `TraitsSingleton` to indicate that this is an internal detail of the traits syntax, however does not take any further arguments. Concretely, it defines the hidden state which is needed to correctly construct the outer and inner functions required to realise the extended dispatch of `@traits`. You don't have to understand it, still you hopefully get the feeling that everything is there.

4. Finally there is `nothing` in order to prevent printing possibly confusing internal details.

If you try `@macroexpand @traits foo(a) where {!isodd(a)} = a/2` instead, you will see that it is very similar, but dispatching on `::Val{false}` instead. This is part of the special support for bool function.

Also try `@macroexpand @traits foo(a) where {iseven(a)} = a/2` and see what the syntax does differently.

-----------

Now execute
```julia
@traits foo(a) where {isodd(a)} = (a+1)/2
```
and repeat inspecting
`@macroexpand @traits foo(a) where {!isodd(a)} = a/2` vs `@macroexpand @traits foo(a) where {iseven(a)} = a/2`.

Also try inspecting methods with other outerfunctions, like `@macroexpand @traits foo(a, b) = a + b`. You will start appreciating the hidden complexity behind the `@traits` syntax.

While the syntax mapping to an outerfunction and respective innerfunctions feels very intuitive, the needed implementation is surprisingly complicated. Luckily, all this is encapsulated nicely in the `@traits` macro. Enjoy!

------------

If you ever are curious what the whole implementation of your `@traits` function is, there is a helper macro `@traits_show_implementation`.
For instance if you finally defined
```julia
@traits foo(a) where {isodd(a)} = (a+1)/2
@traits foo(a) where {!isodd(a)} = a/2
```
`@traits_show_implementation` will give you the full implementation, omitting the internal state.

```juliarepl
julia> @traits_show_implementation foo
  Outer function for signature Tuple{WhereTraits.Syntax.Parsing._BetweenCurliesAndArgs,Any}

  function foo(a1::T1; kwargs...) where T1
      #= none:1 =#
      (Main).foo(WhereTraits.InternalState.TraitsSingleton(), Tuple{WhereTraits.Syntax.Parsing._BetweenCurliesAndArgs,Any}, a1, WhereTraits.Syntax.Rendering._BetweenArgsAndTypeVars(), T1, WhereTraits.Syntax.Rendering._BetweenTypeVarsAndTraits(), Val{isodd(a1)}(); kwargs...)
  end)

    •      •      •  

  Inner functions for signature Tuple{WhereTraits.Syntax.Parsing._BetweenCurliesAndArgs,Any}

  function foo(::WhereTraits.InternalState.TraitsSingleton, ::Type{Tuple{WhereTraits.Syntax.Parsing._BetweenCurliesAndArgs,Any}}, a, ::WhereTraits.Syntax.Rendering._BetweenArgsAndTypeVars, var"'T1'"::Any, ::WhereTraits.Syntax.Rendering._BetweenTypeVarsAndTraits, var"'Val{isodd(a1)}()'"::Val{true})
      #= none:1 =#
      (a + 1) / 2
  end

    •      •      •  

  function foo(::WhereTraits.InternalState.TraitsSingleton, ::Type{Tuple{WhereTraits.Syntax.Parsing._BetweenCurliesAndArgs,Any}}, a, ::WhereTraits.Syntax.Rendering._BetweenArgsAndTypeVars, var"'T1'"::Any, ::WhereTraits.Syntax.Rendering._BetweenTypeVarsAndTraits, var"'Val{isodd(a1)}()'"::Val{false})
      #= none:1 =#
      a / 2
  end

    •      •      •  

  ────────────────────────────────────────────────────────────────
```



## Performance + Comparison with [mauro3/SimpleTraits.jl](https://github.com/mauro3/SimpleTraits.jl)

For a high-level comparison between `WhereTraits.@traits` and `SimpleTraits.jl` see the respective [discourse discussion](https://discourse.julialang.org/t/announcing-traits-jl-a-revival-of-julia-traits/35683/5?u=schlichtanders).

The following examples mirror https://github.com/mauro3/SimpleTraits.jl#details-of-method-dispatch.
We start with defining a custom function `fn`, a custom Trait function `isTr` and some methods for `fn` dispatching on the trait.
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

## WhereTraits.BasicTraits

For anology with SimpleTraits.jl, this package comes with standard traits definitions
`ismutable`, `isimmutable`, `isiterable`, `iscallable`, `isbitstype`, `isconcretetype`. They mostly just wrap respective standard definitions in `Base`, with the added benefit, that they behave similarly to `Base.eltype` in that they have the convenience fallback `ismutable(value) = ismutable(typeof(value))`.

You can use them by executing the following
```julia
using WhereTraits
using WhereTraits.BasicTraits
WhereTraits.BasicTraits.@overwrite_Base

ismutable(1)  # false
ismutable("string")  # true
ismutable(String)  # true
```

Please consult the file [test/BasicTraits.jl](https://github.com/schlichtanders/WhereTraits.jl/blob/master/test/BasicTraits.jl) for more examples.


## Dispatch on whether functions are defined - using [IsDef.jl](https://github.com/schlichtanders/IsDef.jl)

You want to dispatch on whether a function is defined or not? I guess this is a standard scenario and hence I tried to support it, and extracted it into another package.

[IsDef.jl](https://github.com/schlichtanders/IsDef.jl) exports two functions `isdef` and `Out` with which you can dispatch on whether functions are defined or not.
([IsDef.jl](https://github.com/schlichtanders/IsDef.jl) is a sub-dependency of WhereTraits.jl, so you should already have it installed).
With `IsDef.isdef`/`IsDef.Out` and `WhereTraits.@traits` we can define typesafe dispatch like follows:

```julia
using WhereTraits
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

Currently the `@traits` macro has some known restrictions which are soon to be solved:
* the extended where syntax is currently implemented on **symbol level**, which is why traits functions like `Base.IteratorSize` and the non-qualified `IteratorSize` (assuming you imported `import Base:IteratorSize`) are treated as two different functions, despite being the same. So for now try to only use the one style or the other.

    On possibility would be to fix this by looking up method definitions in the caller module.

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
* The `@traits` currently does not work well within the `Test.@testset` macro. As a workaround WhereTraits.jl exports a `@traits_test` variant which works better, but still has cases where it fails. This needs to be investigated further, and maybe needs a fix on `Test.@testset`, don't know.
