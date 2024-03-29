
# WhereTraits Usage

## Usage `@traits`

`@traits` supports the following three extensions to Julia's where-syntax:
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
- dispatch on upperbounds on functions returning Types
```julia
@traits h(a) where {eltype(a) <: Number} = true
@traits h(a) = false
h([1.0]) # true
h([""]) # false
```

And all this works with arbitrary many where expressions and creates optimal code where possible via standard Julia compiler.

## Usage `@traits_order` - Resolving Ambiguities

`WhereTraits` comes with special support for resolving ambiguities among traits dispatch. 

Under the hood `@traits` uses normal function dispatch to acchieve the speed and flexibility, however, julia function dispatch can lead to disambiguities. With traits these can easily happen if someone defines `@traits` for the same standard dispatch but using different traits. Let's take a look how this looks like

```julia
using WhereTraits
# let's say someone defined this version
@traits conflict(a) where {eltype(a) <: Number} = "eltype wins"
# and another this one
@traits conflict(a) where {Base.IteratorSize(a) :: Base.HasShape} = "IteratorSize wins"
```

You can still use these traits definition as long as there is no ambiguity.
```julia
julia> conflict(Iterators.countfrom(42))
"eltype wins"

julia> conflict(["hello", "world"])
"IteratorSize wins"
```

If you use something ambiguous, e.g. a `Vector` of `Number`s, you get a proper ambiguity error, stating what you can do in order to fix it.
```julia
julia> conflict([1,2,3,4])
ERROR: Disambiguity found. Please specify an ordering between traits, like the following.

    @traits_order (Main).conflict(a1::T1) where T1 begin
        eltype(a1)
        Base.IteratorSize(a1)
    end

Stacktrace:
[...]
```

What is needed in order to resolve the ambiguity is an order between the traits. This can be defined with `@traits_order`, which takes the respective function signature followed by a begin-end block of ordered traits (most dominant one should be at the top).

Hence just executing the example `@traits_order` will make `eltype` be the winning trait.
```julia
@traits_order (Main).conflict(a1::T1) where T1 begin
    eltype(a1)
    Base.IteratorSize(a1)
end
```

Let's take a look that everything is resolved
```julia
julia> conflict([1,2,3,4])
"eltype wins"
```

Alternatively to the `@traits_order` you can always define your own custom resolution

```julia
@traits function conflict(a) where {eltype(a) <: Number, Base.IteratorSize(a) :: Base.HasShape}
    "custom implementation"
end
```

which immediately will resolve correctly
```julia
julia> conflict([1,2,3,4])
"custom implementation"
```

For more details, take a look at the [documentation](https://jolin-io.github.io/WhereTraits.jl/dev).


## Limitations

### Optimal Code
*Warning: While the dispatch works for dynamic functions, it will only be able to create optimal code if your traits function supports proper type-inference. E.g. you can use `Base.isempty`, however type-inference cannot see whether it will return true or false by static inspection. Hence it will use slower dynamic code.*

### Keyword arguments
Keyword arguments are at the moment not support for WhereTraits dispatch. They are just passed through.

### Symbol Level

The extended where syntax is currently implemented on **symbol level**, which is why traits functions like `Base.IteratorSize` and the non-qualified `IteratorSize` (assuming you imported `import Base: IteratorSize`) are treated as two different functions, despite being the same. So for now try to only use the one style or the other.

### Top Level Only
Currently **only top-level functions** are supported, as the syntax stores and needs information about previous function definitions, which it stores globally. If macros would get informed about whether they are defined within another function, WhereTraits could also support innerfunctions. 

###  Test package
The `@traits` macro currently does not work well within the `Test.@testset` macro. Usually you won't encounter this, as standard dispatch is probably enough for your tests.

Nevertheless there is a workaround. WhereTraits.jl exports a `@traits_test` macro variant which works better, but still might have cases where it fails. This needs to be investigated further, and maybe needs a change on `Test.@testset`.
