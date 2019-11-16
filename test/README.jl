using Traits
using Test

using Suppressor
using StringDistances  # TODO make this a submodule for test only

# Test README.md example
# ======================


isTr(_) = false

fn(x::Integer) = 1
@traits fn(x::X) where {X<:AbstractFloat, isTr(X)} = 2
@traits_show_implementation fn

@traits fn(x::AbstractFloat) = 3
@traits_show_implementation fn

if !isTr(Float32)  # if the test is reexecuted, the definition already exists
  @test fn(Float32(5)) == 3 # -> 3; dispatch through traits, but isTr not yet defined, hence using default case
end

isTr(::Type{Float32}) = true
isTr(::Type{Int}) = true

@test fn(5) == 1 # -> 1; dispatch only happens on the type
@test fn(Float32(5)) == 2 # -> 2; dispatch through traits
@test fn(Float64(5)) == 3 # -> 3; default dispatch through traits


isTr2(_) = false
isTr2(::Type{Float16}) = true
@traits fn(x::X) where {X <: AbstractFloat, isTr2(X)} = 4
@traits_show_implementation fn

@test fn(Float16(5)) == 4 # -> 4; dispatch through traits
@test fn(Float32(5)) == 2 # -> 2; NO MethodError; nothing is overwritten, everything works like you would hope for


out1 = @capture_out begin
  @code_llvm fn(5)
end
out2 = @capture_out begin
  @code_llvm fn(Float16(5))
end

@code_llvm fn(Float32(5))

function find_definition(out)
  s = findfirst("Function", out)
  out[s.start: end]
end

# string distance should be small
@test evaluate(Levenshtein(), find_definition(out1), find_definition(out2)) <= 10
# println(out1)
# println(out2)
@code_warntype fn(5)



# isdef, Out
# ==========

struct MyError <: Exception
  msg::AbstractString
end
MyError() = MyError("")

@traits typesafe_call(f, a::T) where {T, isdef(f, T)} = f(a)
# not that you can also use isdef on args directly, like you can use eltype on args
# however this will be recognized as a different traitfunction compared to isdef(f, T)
# which can lead to ambiguous overloadings. Just good to keep in mind
@traits typesafe_call(f, a) where {!isdef(f, a)} = throw(MyError("given function cannot work with given arg"))
@code_llvm typesafe_call(x -> x+2, 4)  # looks really good

@test typesafe_call(x -> x+2, 4) == 6
@test_throws MyError typesafe_call(x -> x+2, "string")
@traits_show_implementation typesafe_call


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
