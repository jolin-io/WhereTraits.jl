using Traits
using Test
using ASTParser

# Test standard dispatch
# ======================
@macroexpand @traits g(a) = a

@testset "standard dispatch" begin
  # s === standard
  @traits_delete! fs  # we need to delete because @testset uses closures which confuse the state of @traits
  @traits_test fs(a::A) where {A} = A
  @test fs(1) == Int
  @test fs("hi") == String
  @test_throws MethodError fs(1, 2)

  @traits_delete! fs2  # we need to delete because @testset uses closures which confuse the state of @traits
  @traits_test fs2(a::Vector{A}) where {Integer<:A<:Number} = sum(a)
  @test fs2(convert(Vector{Integer}, [1,2,3])) == 6
  @test_throws MethodError fs2([1,2,3])
end

# Test boolean dispatch
# =====================

@testset "bool dispatch" begin
  # b === bool
  @traits_delete! fb  # we need to delete because @testset uses closures which confuse the state of @traits
  @traits_test fb(a::Vector) where {!isempty(a)} = a[1]
  @traits_test fb(a::Vector) where {isempty(a)} = nothing
  @traits_show_implementation fb

  @test isnothing(fb([]))
  @test fb([43]) == 43
  @test_throws MethodError fb((1,2,3))

  @traits_delete! fb2  # we need to delete because @testset uses closures which confuse the state of @traits
  @traits_test fb2(a::Vector{A}) where {A, isconcretetype(A)} = length(a)
  @traits_test fb2(a::Vector{A}) where {A, !isconcretetype(A)} = length(a) + 1

  @test fb2([]) == 1  # Any is not concrete, length == 0
  @test fb2([3,5,.6]) == 3  # Float is concrete, length == 3
  @test_throws MethodError fb2((1,2,3))

  @traits_delete! fb3  # we need to delete because @testset uses closures which confuse the state of @traits
  @traits_test fb3(a) where {a} = 1
  @traits_test fb3(a) where {!a} = 2

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
  @traits_test fcc(a::A) where {A, Base.IteratorSize(A)::Base.HasLength} = length(a)
  @traits_test fcc(a::A) where {A, Base.IteratorSize(A)::Base.HasShape} = size(a)
  @traits_test fcc(a::A) where {A, Base.IteratorSize(A)::Base.SizeUnknown} = nothing
  @traits_show_implementation fcc

  @test fcc(1:4) == (4,)
  @test fcc("hallo") == 5
  @test_throws MethodError fcc(Base.Iterators.repeated(2))  # IteratorSize == Infinite

  @traits_delete! fcc2  # we need to delete because @testset uses closures which confuse the state of @traits
  @traits_test fcc2(a) where {eltype(a)::Type{Int}} = 5 + sum(a)
  @traits_test fcc2(a) where {eltype(a)::Type{String}} = "[$(join(a, ","))]"
  @test fcc2([1,2,3,4]) == 15
  @test fcc2(["a", "b"]) == "[a,b]"
  @test_throws MethodError fcc2([2.0, 5.0])

  @traits_delete! fcc3  # we need to delete because @testset uses closures which confuse the state of @traits
  @traits_test fcc3(a::Val{T}) where {T, T::Int} = T + 4
  @traits_test fcc3(a::Val{T}) where {T, T::Symbol} = T
  @test fcc3(Val(1)) == 5
  @test fcc3(Val(:hi)) == :hi
  @test_throws MethodError fcc3(Val(true))
end


# Test <: dispatch
# ================

@testset "<: dispatch" begin
  #sc === smaller colon
  @traits_delete! fsc  # we need to delete because @testset uses closures which confuse the state of @traits
  @traits_test fsc(a::A) where {A, eltype(A)<:Int} = 5 + sum(a)
  @traits_test fsc(a::A) where {A, eltype(A)<:String} = "[$(join(a, ","))]"
  @test fsc([1,2,3,4]) == 15
  @test fsc(["a", "b"]) == "[a,b]"
  @test_throws MethodError fsc([2.0, 5.0])

  @traits_delete! fsc2  # we need to delete because @testset uses closures which confuse the state of @traits
  @traits_test fsc2(a) where {eltype(a)<:Int} = 5 + sum(a)
  @traits_test fsc2(a) where {eltype(a)<:String} = "[$(join(a, ","))]"
  @test fsc2([1,2,3,4]) == 15
  @test fsc2(["a", "b"]) == "[a,b]"
  @traits_show_implementation fsc2
  @test_throws MethodError fsc2([2.0, 5.0])

  @traits_delete! fsc2  # we need to delete because @testset uses closures which confuse the state of @traits
  @traits_test fsc2(a) where {eltype(a)<:Int} = 5 + sum(a)
  @traits_test fsc2(a) where {eltype(a)<:String} = "[$(join(a, ","))]"
  @test fsc2([1,2,3,4]) == 15
  @test fsc2(["a", "b"]) == "[a,b]"
  @traits_show_implementation fsc2
  @test_throws MethodError fsc2([2.0, 5.0])
end

# Test default
# ============

@testset "default args" begin
  @traits_delete! fda
  @traits_test fda(a, b = 3) = a + b
  @traits_show_implementation fda
  @test fda(1) == 4

  # defaults will be preserved because they have been translated to extra methods
  # which are currently not tracked by @traits_test due to complexity
  # this is the same behaviour as for standard julia functions
  @traits_test fda(a, b) = a + b + 9
  @test fda(1) == 13
  @test_throws MethodError fda()

  # but they can be overwritten as usual
  @traits_test fda(a, b=1) = a + b + 99
  @test fda(1) == 101

  # or more drastically by an overwrite
  @traits_test fda(a) = 3 + a
  @test fda(1) == 4

  # also reassigning default values will overwrite main clause
  @traits_test fda(a, b=50) = a + b + 999
  @test fda(1) == 1050

  # check that also traits get lowered correctly
  @traits_delete! fda2
  @traits_test fda2(a::A, b::B=50) where {A<:Number, B<:Number, eltype(A) == Int, A == B, eltype(b) <: Number} = a + b + 999
  @traits_show_implementation fda2
  @test fda2(1) == 1050  # should not throw "UndefVarError: B not defined"
  @test_throws MethodError fda2(1.0)
  @test_throws MethodError fda2(1.0, 4)
end

# Test kwargs
# ===========

@testset "default kwargs" begin
  @traits_delete! fdkw
  # kwargs are not used for standard julia dispatch, and neither for traits dispatch
  # they can be seen as a list of extra configurations for a concrete dispatch
  @traits_test fdkw(a; b = 3) = a + b
  @traits_show_implementation fdkw
  @test fdkw(1) == 4

  @traits_test fdkw(a; b=1) = a + b + 99
  @test fdkw(1) == 101

  # or more drastically by an overwrite
  @traits_test fdkw(a) = 3 + a
  @test fdkw(1) == 4

  # overwriting works
  @traits_test fdkw(a; b=50) = a + b + 999
  @test fdkw(1) == 1050

  # or more drastically by an overwrite
  @traits_test fdkw(a) = 3 + a
  @test fdkw(1) == 4

  # overwriting again works too
  @traits_test fdkw(a; b=50) = a + b + 999
  @test fdkw(1) == 1050
end


# Test overwritability
# ====================

@testset "overwritability" begin
  @traits_delete! ow  # we need to delete because @testset uses closures which confuse the state of @traits
  @traits_test ow(a::A) where {A, IteratorSize(A)::Base.HasLength} = length(a)
  @traits_test ow(a::A) where {A, IteratorSize(A)::Base.HasShape} = shape(a)
  @traits_test ow(a::A) where {A, IteratorSize(A)::Base.SizeUnknown} = 3
  expr1 = @traits_show_implementation ow
  # @test length(expr1.args) == 4
  @traits_test ow(a::A) where {A, IteratorSize(A)::Base.SizeUnknown} = 8
  expr2 = @traits_show_implementation ow
  # @test length(expr2.args) == 4
  @test length(expr1.args) == length(expr2.args)

  @traits_delete! ow
  expr3 = @traits_show_implementation ow
  @test length(expr3.args) == 0

  @traits_test ow(a::A) where {A, Base.IteratorSize(A)::Base.HasLength} = length(a)
  @traits_test ow(a::A) where {A, Base.IteratorSize(A)::Base.HasShape} = size(a)
  @traits_test ow(a::A) where {A, Base.IteratorSize(A)::Base.SizeUnknown} = nothing
  expr4 = @traits_show_implementation ow
  @test length(expr1.args) == length(expr4.args)
end

# Test varargs and varkwargs
# ==========================

@testset "Vararg varkwarg" begin
  # super default case
  @traits_delete! f
  @traits_test f(args...; kwargs...) = (args, kwargs)
  @traits_show_implementation f

  a, k = f(1,2,3,4, a=4, b=5)
  @test a == (1,2,3,4)
  @test k[:a] == 4
  @test k[:b] == 5
  @test sort(collect(keys(k))) == [:a, :b]
end


# Test complex type dispatch
# ==========================

# Tuple{Tuple{Tuple{Vector{Some}}, Vararg}}

@testset "complex dispatch" begin
  @traits_delete! g
  @traits_test g(a::Tuple{Number, Tuple{Int, Vector}}) = 1

  @traits_test function g(a::Tuple{Number, Tuple{Int, T}}) where {T <: Vector, eltype(T)::Type{Int}}
    2
  end
  @test g((1.0, (2, [:a, :b]))) == 1
  @test g((1.0, (2, [1, 2]))) == 2
end

# TODO add case with Union type
# TODO add case for diagonal dispatch `f(a::T, b::T) where T`
# TODO add case for arg without name

# TODO add case for function in submodule like ``@traits_test mysubmodule.myfunc(a) = 4``
# TODO add case for overwriting same target function from different modules like ``@traits Target.myfunc(...)`` from
# both module A and module B
