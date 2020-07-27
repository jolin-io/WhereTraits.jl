# WhereTraits.jl


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


*Warning: While the dispatch works for dynamic functions, it will only be able to create optimal code if your traits function supports proper type-inference. E.g. you can use `Base.isempty`, however type-inference cannot see whether it will return true or false by static inspection. Hence it will use slower dynamic code.*


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


## Current Restrictions and Future Plans

Currently the `@traits` macro has some known restrictions:
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


## Manual Outline

```@contents
Pages = ["manual.md"]
```

## [Library Index](@id main-index)

```@index
Pages = ["library.md"]
```
