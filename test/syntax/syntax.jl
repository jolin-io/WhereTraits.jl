using Traits
using Test
using ASTParser

@testset "Utils" begin
  include("utils.jl")
end

# Test standard dispatch
# ======================
@testset "standard dispatch" begin
  # s === standard
  @traits_delete! fs  # we need to delete because @testset uses closures which confuse the state of @traits
  @traits fs(a::A) where {A} = A
  @traits_show_implementation fs

  @test fs(1) == Int
  @test fs("hi") == String
  @test_throws MethodError fs(1, 2)

  @traits_delete! fs2  # we need to delete because @testset uses closures which confuse the state of @traits
  @traits fs2(a::Vector{A}) where {Integer<:A<:Number} = sum(a)
  @test fs2(convert(Vector{Integer}, [1,2,3])) == 6
  @test_throws MethodError fs2([1,2,3])
end

# Test boolean dispatch
# =====================
@testset "bool dispatch" begin
  # b === bool
  @traits_delete! fb  # we need to delete because @testset uses closures which confuse the state of @traits
  @traits fb(a::Vector) where {!isempty(a)} = a[1]
  @traits fb(a::Vector) where {isempty(a)} = nothing
  @traits_show_implementation fb

  @test isnothing(fb([]))
  @test fb([43]) == 43
  @test_throws MethodError fb((1,2,3))

  @traits_delete! fb2  # we need to delete because @testset uses closures which confuse the state of @traits
  @traits fb2(a::Vector{A}) where {A, isconcretetype(A)} = length(a)
  @traits fb2(a::Vector{A}) where {A, !isconcretetype(A)} = length(a) + 1

  @test fb2([]) == 1  # Any is not concrete, length == 0
  @test fb2([3,5,.6]) == 3  # Float is concrete, length == 3
  @test_throws MethodError fb2((1,2,3))

  @traits_delete! fb3  # we need to delete because @testset uses closures which confuse the state of @traits
  @traits fb3(a) where {a} = 1
  @traits fb3(a) where {!a} = 2

  @traits_show_implementation fb3

  @test fb3(true) == 1
  @test fb3(false) == 2
  @test_throws MethodError fb3(true, false)
end


# Test :: dispatch
# ================

@testset ":: dispatch" begin
  #cc === colon colon
  @traits_delete! fcc  # we need to delete because @testset uses closures which confuse the state of @traits
  @traits fcc(a::A) where {A, Base.IteratorSize(A)::Base.HasLength} = length(a)
  @traits fcc(a::A) where {A, Base.IteratorSize(A)::Base.HasShape} = size(a)
  @traits fcc(a::A) where {A, Base.IteratorSize(A)::Base.SizeUnknown} = nothing
  @traits_show_implementation fcc

  @test fcc(1:4) == (4,)
  @test fcc("hallo") == 5
  @test_throws MethodError fcc(Base.Iterators.repeated(2))  # IteratorSize == Infinite

  @traits_delete! fcc2  # we need to delete because @testset uses closures which confuse the state of @traits
  @traits fcc2(a) where {eltype(a)::Type{Int}} = 5 + sum(a)
  @traits fcc2(a) where {eltype(a)::Type{String}} = "[$(join(a, ","))]"
  @test fcc2([1,2,3,4]) == 15
  @test fcc2(["a", "b"]) == "[a,b]"
  @test_throws MethodError fcc2([2.0, 5.0])

  @traits_delete! fcc3  # we need to delete because @testset uses closures which confuse the state of @traits
  @traits fcc3(a::Val{T}) where {T, T::Int} = T + 4
  @traits fcc3(a::Val{T}) where {T, T::Symbol} = T
  @test fcc3(Val(1)) == 5
  @test fcc3(Val(:hi)) == :hi
  @test_throws MethodError fcc3(Val(true))
end


# Test <: dispatch
# ================

@testset "<: dispatch" begin
  #sc === smaller colon
  @traits_delete! fsc  # we need to delete because @testset uses closures which confuse the state of @traits
  @traits fsc(a::A) where {A, eltype(A)<:Int} = 5 + sum(a)
  @traits fsc(a::A) where {A, eltype(A)<:String} = "[$(join(a, ","))]"
  @test fsc([1,2,3,4]) == 15
  @test fsc(["a", "b"]) == "[a,b]"
  @test_throws MethodError fsc([2.0, 5.0])

  @traits_delete! fsc2  # we need to delete because @testset uses closures which confuse the state of @traits
  @traits fsc2(a) where {eltype(a)<:Int} = 5 + sum(a)
  @traits fsc2(a) where {eltype(a)<:String} = "[$(join(a, ","))]"
  @test fsc2([1,2,3,4]) == 15
  @test fsc2(["a", "b"]) == "[a,b]"
  @traits_show_implementation fsc2
  @test_throws MethodError fsc2([2.0, 5.0])

  @traits_delete! fsc2  # we need to delete because @testset uses closures which confuse the state of @traits
  @traits fsc2(a) where {eltype(a)<:Int} = 5 + sum(a)
  @traits fsc2(a) where {eltype(a)<:String} = "[$(join(a, ","))]"
  @test fsc2([1,2,3,4]) == 15
  @test fsc2(["a", "b"]) == "[a,b]"
  @traits_show_implementation fsc2
  @test_throws MethodError fsc2([2.0, 5.0])
end

# Test default
# ============

@testset "default args" begin
  @traits_delete! fda
  @traits fda(a, b = 3) = a + b
  @traits_show_implementation fda
  @test fda(1) == 4

  # defaults will be preserved because they have been translated to extra methods
  # which are currently not tracked by @traits due to complexity
  # this is the same behaviour as for standard julia functions
  @traits fda(a, b) = a + b + 99
  @test fda(1) == 103
  @test_throws MethodError fda()

  # but they can be overwritten as usual
  @traits fda(a, b=1) = a + b + 99
  @test fda(1) == 101

  # or more drastically by an overwrite
  @traits fda(a) = 3 + a
  @test fda(1) == 4

  # now default
  @traits fda(a, b=50) = a + b + 99
  @test fda(1) == 101
end

# Test kwargs
# ===========


@testset "default kwargs" begin
  @traits_delete! fdkw
  # kwargs are not used for standard julia dispatch, and neither for traits dispatch
  # they can be seen as a list of extra configurations for a concrete dispatch
  @traits fdkw(a; b = 3) = a + b
  @traits_show_implementation fdkw
  @test fdkw(1) == 4

  @traits fdkw(a; b=1) = a + b + 99
  @test fdkw(1) == 101

  # or more drastically by an overwrite
  @traits fdkw(a) = 3 + a
  @test fdkw(1) == 4

  # overwriting works
  @traits fdkw(a; b=50) = a + b + 99
  @test fdkw(1) == 150

  # or more drastically by an overwrite
  @traits fdkw(a) = 3 + a
  @test fdkw(1) == 4
end

# Test complex interactions (also with standard dispatch)
# =======================================================

# Test overwritability
# ====================

@testset "overwritability" begin
  @traits_delete! ow  # we need to delete because @testset uses closures which confuse the state of @traits
  @traits ow(a::A) where {A, IteratorSize(A)::Base.HasLength} = length(a)
  @traits ow(a::A) where {A, IteratorSize(A)::Base.HasShape} = shape(a)
  @traits ow(a::A) where {A, IteratorSize(A)::Base.SizeUnknown} = 3
  expr1 = @traits_show_implementation ow
  # @test length(expr1.args) == 4
  @traits ow(a::A) where {A, IteratorSize(A)::Base.SizeUnknown} = 8
  expr2 = @traits_show_implementation ow
  # @test length(expr2.args) == 4
  @test length(expr1.args) == length(expr2.args)

  @traits_delete! ow
  expr3 = @traits_show_implementation ow
  @test length(expr3.args) == 0

  @traits ow(a::A) where {A, Base.IteratorSize(A)::Base.HasLength} = length(a)
  @traits ow(a::A) where {A, Base.IteratorSize(A)::Base.HasShape} = size(a)
  @traits ow(a::A) where {A, Base.IteratorSize(A)::Base.SizeUnknown} = nothing
  expr4 = @traits_show_implementation ow
  @test length(expr1.args) == length(expr4.args)
end


# test internal details
# =====================

d1 = Dict(:k => 4, :a => 5)
d2 = Dict(:k => 6, :a => nodefault)
Traits.Syntax._merge_args_defaults(d1, d2) == Dict{Symbol, Any}(:k => 6, :a => 5)
