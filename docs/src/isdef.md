

# Dispatch on whether functions are defined - using [IsDef.jl](https://github.com/schlichtanders/IsDef.jl)

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
