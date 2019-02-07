using Traits
using Test

@traitsof_init

@testset "README" begin
  include("README.jl")
end

@testset "BasicTraits" begin
  include("BasicTraits.jl")
end


struct TrA1 end

@testset "arity1 no parametertypes" begin
  traitsof_refixate()

  fA1(a) = fA1(traitsof(a), a)
  fA1(::TypeLB(TrA1), a) = 42
  fA1(_, a) = 0

  @test fA1(Int) == 0
  traitsof[Int] = TrA1
  @test fA1(Int) == 0
  # we need to call traitsof_refixate(), as internally within the test ``fA1(Int)``
  # the ``traitsof(Int)`` was asked for and hence is generated without the additional Trait TrA1
  traitsof_refixate()
  @test fA1(Int) == 42

  @testset "supertypes" begin
    @test fA1(String) == 0
    traitsof[AbstractString] = TrA1  # you can assign Traits to supertypes as well
    traitsof_refixate()
    @test fA1(String) == 42
  end
end

struct TrA1Dict end
struct TrA1DictConstrain1 end

struct TrA1_1 end
struct TrA1Dict_fancy end
struct TrA1DictConstrain1_fancy end

# careful: on re-executing this several tests will be skipped, Traits that should be missing will be already defined
@testset "arity1 with parametertypes" begin
  traitsof_refixate()

  traitsof[Dict] = TrA1Dict
  traitsof[Constrain1{Dict}(Union{}, TrA1)] = TrA1DictConstrain1  # use Union{} as all matching default value

  @test TrA1Dict <: traitsof(Dict{Int, String})
  if !(TrA1 <: traitsof[String])
    @test !(TrA1DictConstrain1 <: traitsof(Dict{Int, String}))  # not working because String has no TrA1 trait
    traitsof[String] = TrA1
    @test !(TrA1DictConstrain1 <: traitsof(Dict{Int, String}))  # still not working, because `traitsof(String)` was already fixated
    traitsof_refixate()
  end
  @test TrA1DictConstrain1 <: traitsof(Dict{Int, String})  # now it works :-)

  @testset "unionall constraints" begin

    traitsof[Dict{<:Number, <:AbstractString}] = TrA1Dict_fancy
    traitsof[Constrain1{Dict{<:Number, <:AbstractString}}(TrA1_1, TrA1)] = TrA1DictConstrain1_fancy
    traitsof[Int] = TrA1_1
    traitsof_refixate()

    @test TrA1Dict_fancy <: traitsof(Dict{Int, String})
    @test !(TrA1Dict_fancy <: traitsof(Dict{String, String}))
    @test !(TrA1Dict_fancy <: traitsof(Dict{Int, Bool}))

    @test TrA1DictConstrain1_fancy <: traitsof(Dict{Int, String})
    if !(TrA1_1 <: traitsof(Bool))
      @test !(TrA1DictConstrain1_fancy <: traitsof(Dict{Bool, String}))  # we already defined TrA1_3 for all Integer
      traitsof[Integer] = TrA1_1
      traitsof_refixate()
    end
    @test TrA1DictConstrain1_fancy <: traitsof(Dict{Bool, String})
  end
end

struct TrA2 end

@testset "arity2 no parametertypes" begin
  traitsof_refixate()

  fA2(a, b) = fA2(traitsof(a, b), a, b)
  fA2(::TypeLB(TrA2), a, b) = 42
  fA2(_, a, b) = 0

  @test fA2(Int, String) == 0
  traitsof[Int, String] = TrA2
  @test fA2(Int, String) == 0
  traitsof_refixate()
  @test fA2(Int, String) == 42

  @testset "supertypes" begin
    @test fA2(Int, Bool) == 0
    traitsof[Int, Integer] = TrA2  # Bool <: Integer
    traitsof_refixate()
    @test fA2(Int, Bool) == 42
  end
end

struct TrA2_1 end
struct TrA2_2 end
struct TrA2VectorDict end
struct TrA2VectorDictConstrain2 end
struct TrA2VectorDict_fancy end
struct TrA2VectorDictConstrain2_fancy end

@testset "arity2 with parametertypes" begin
  traitsof_refixate()

  traitsof[Vector, Dict] = TrA2VectorDict
  traitsof[Constrain2{Vector, Dict}(TrA2_1, Union{}, Union{}, TrA2_2)] = TrA2VectorDictConstrain2  # use Union{} as all matching default value

  @test TrA2VectorDict <: traitsof(Vector{Int}, Dict{Int, String})
  traitsof[String]
  @test !(TrA2VectorDictConstrain2 <: traitsof(Vector{Int}, Dict{Int, String}))  # not working because String has no TrA2 trait
  traitsof[String] = TrA2_2
  traitsof_refixate()
  @test !(TrA2VectorDictConstrain2 <: traitsof(Vector{Int}, Dict{Int, String}))  # still not working, String has no trait TrA2_1
  traitsof[Int] = TrA2_1
  traitsof_refixate()
  @test TrA2VectorDictConstrain2 <: traitsof(Vector{Int}, Dict{Int, String})  # works, as both Traits constraints are fulfilled
  @test !(TrA2VectorDictConstrain2 <: traitsof(Vector{String}, Dict{Int, String}))  # String does not have TrA2_1

  @testset "unionall constraints" begin
    traitsof[Vector{<:Number} , Dict{T, <:AbstractString} where T] = TrA2VectorDict_fancy
    traitsof[Constrain2{Vector{<:Number} , Dict{T, <:AbstractString} where T}(TrA2_1, Union{}, Union{}, TrA2_2)] = TrA2VectorDictConstrain2_fancy
    traitsof_refixate()

    @test TrA2VectorDict_fancy <: traitsof(Vector{Int}, Dict{Int, String})
    @test !(TrA2VectorDict_fancy <: traitsof(Vector{String}, Dict{Int, String}))
    @test !(TrA2VectorDict_fancy <: traitsof(Vector{Int}, Dict{Int, Bool}))

    @test TrA2VectorDictConstrain2_fancy <: traitsof(Vector{Int}, Dict{Int, String})
    @test !(TrA2VectorDictConstrain2_fancy <: traitsof(Vector{Bool}, Dict{Int, String}))  # we already defined TrA2_3 for all Integer
    traitsof[Integer] = TrA2_1
    traitsof_refixate()
    @test TrA2VectorDictConstrain2_fancy <: traitsof(Vector{Bool}, Dict{Int, String})
  end
end


struct TrA3 end

@testset "arity3 no parametertypes" begin
  traitsof_refixate()

  fA3(a, b, c) = fA3(traitsof(a, b, c), a, b, c)
  fA3(::TypeLB(TrA3), a, b, c) = 42
  fA3(_, a, b, c) = 0

  @test fA3(Int, String, Bool) == 0
  traitsof[Int, String, Bool] = TrA3
  @test fA3(Int, String, Bool) == 0
  traitsof_refixate()
  @test fA3(Int, String, Bool) == 42

  @testset "supertypes" begin
    @test fA3(Int, String, Float32) == 0
    traitsof[Int, String, AbstractFloat] = TrA3
    traitsof_refixate()
    @test fA3(Int, String, Float32) == 42
  end
end

struct TrA3_1 end
struct TrA3_2 end
struct TrA3_3 end
struct TrA3VectorDictDict end
struct TrA3VectorDictDictConstrain3 end

struct TrA3VectorDictDict_fancy end
struct TrA3VectorDictDictConstrain3_fancy end

@testset "arity3 with parametertypes" begin
  traitsof_refixate()

  traitsof[Vector, Dict, Dict] = TrA3VectorDictDict
  traitsof[Constrain3{Vector, Dict, Dict}(TrA3_1, Union{}, Union{}, TrA3_2, TrA3_3, Union{})] = TrA3VectorDictDictConstrain3  # use Union{} as all matching default value

  @test TrA3VectorDictDict <: traitsof(Vector{Int}, Dict{Int, String}, Dict{Int, Int})
  @test !(TrA3VectorDictDictConstrain3 <: traitsof(Vector{Int}, Dict{Int, String}, Dict{Int, Int}))  # not working because String has no TrA3 trait
  traitsof[Int] = TrA3_1
  traitsof[String] = TrA3_2
  traitsof[Integer] = TrA3_3
  traitsof_refixate()
  @test TrA3VectorDictDictConstrain3 <: traitsof(Vector{Int}, Dict{Int, String}, Dict{Int, Int})  # works, as both Traits constraints are fulfilled
  traitsof[AbstractString] = TrA3_1
  traitsof_refixate()
  @test TrA3VectorDictDictConstrain3 <: traitsof(Vector{String}, Dict{Int, String}, Dict{Bool, Int})  # still works as Bool is also a Number


  @testset "unionall constraints" begin
    traitsof[Vector{<:Number} , Dict{T, <:AbstractString} where T, Dict{<:Number}] = TrA3VectorDictDict_fancy
    traitsof[Constrain3{Vector{<:Number} , Dict{T, <:AbstractString} where T, Dict{<:Number}}(TrA3_1, Union{}, Union{}, TrA3_2, TrA3_3, Union{})] = TrA3VectorDictDictConstrain3_fancy
    traitsof_refixate()

    @test TrA3VectorDictDict_fancy <: traitsof(Vector{Int}, Dict{Int, String}, Dict{Int, Int})
    @test !(TrA3VectorDictDict_fancy <: traitsof(Vector{String}, Dict{Int, String}, Dict{Int, Int}))
    @test !(TrA3VectorDictDict_fancy <: traitsof(Vector{Int}, Dict{Int, Bool}, Dict{Int, Int}))

    @test TrA3VectorDictDictConstrain3_fancy <: traitsof(Vector{Int}, Dict{Int, String}, Dict{Int, Int})
    @test TrA3VectorDictDictConstrain3_fancy <: traitsof(Vector{Int}, Dict{Int, String}, Dict{Bool, Int})  # we already defined TrA3_3 for all Integer
    @test !(TrA3VectorDictDictConstrain3_fancy <: traitsof(Vector{Int}, Dict{Int, String}, Dict{Float32, Int}))
    traitsof[AbstractFloat] = TrA3_3
    traitsof_refixate()
    @test TrA3VectorDictDictConstrain3_fancy <: traitsof(Vector{Int}, Dict{Int, String}, Dict{Float32, Int})
  end
end
