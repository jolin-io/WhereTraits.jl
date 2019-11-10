struct TrA1 end

# TODO adapt arity tests to new syntax


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

trait_TrA1DictConstrain1(::Traitsof, x...) = begin
  Union{}
end
function trait_TrA1DictConstrain1(traitsof::Traitsof, ::Type{Dict{K,V}}) where K where V
  if TrA1 <: traitsof(V)
    TrA1DictConstrain1
  else
    Union{}
  end
end

trait_TrA1DictConstrain1_fancy(::Traitsof, _...) = Union{}
function trait_TrA1DictConstrain1_fancy(traitsof::Traitsof, ::Type{Dict{K, V}}) where K<:Number where V<:AbstractString
  if TrA1_1 <: traitsof(K) && TrA1 <: traitsof(V)
    TrA1DictConstrain1_fancy
  else
    Union{}
  end
end

# careful: on re-executing this several tests will be skipped, Traits that should be missing will be already defined
@testset "arity1 with parametertypes" begin
  traitsof_refixate()

  traitsof[Dict] = TrA1Dict
  push!(traitsof, trait_TrA1DictConstrain1)
  @test TrA1Dict <: traitsof(Dict{Int, String})
  if !(TrA1 <: traitsof[String])
    @test !(TrA1DictConstrain1 <: traitsof(Dict{Int, String}))  # not working because String has no TrA1 trait
    traitsof[String] = TrA1
    @test !(TrA1DictConstrain1 <: traitsof(Dict{Int, String}))  # still not working, because `traitsof(String)` was already fixated
    traitsof_refixate()
  end
  @test TrA1DictConstrain1 <: traitsof(Dict{Int, String})  # now it works :-)

  @testset "unionall trait_function" begin

    traitsof[Dict{<:Number, <:AbstractString}] = TrA1Dict_fancy
    push!(traitsof, trait_TrA1DictConstrain1_fancy)
    traitsof[Int] = TrA1_1
    traitsof_refixate()

    @test TrA1Dict_fancy <: traitsof(Dict{Int, String})
    @test !(TrA1Dict_fancy <: traitsof(Dict{String, String}))
    @test !(TrA1Dict_fancy <: traitsof(Dict{Int, Bool}))

    traitsof(String)
    traitsof(Int)
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

trait_TrA2VectorDictConstrain2(::Traitsof, _...) = Union{}
function trait_TrA2VectorDictConstrain2(traitsof::Traitsof, ::Type{Vector{VE}}, ::Type{Dict{DK, DV}}) where VE where DK where DV
  if TrA2_1 <: traitsof(VE) && TrA2_2 <: traitsof(DV)
    TrA2VectorDictConstrain2
  else
    Union{}
  end
end

trait_TrA2VectorDictConstrain2_fancy(::Traitsof, _...) = Union{}
function trait_TrA2VectorDictConstrain2_fancy(traitsof::Traitsof, ::Type{Vector{VE}}, ::Type{Dict{DK, DV}}) where VE where DK where DV<:AbstractString
  if TrA2_1 <: traitsof(VE) && TrA2_2 <: traitsof(DV)
    TrA2VectorDictConstrain2_fancy
  else
    Union{}
  end
end

@testset "arity2 with parametertypes" begin
  traitsof_refixate()

  traitsof[Vector, Dict] = TrA2VectorDict
  push!(traitsof, trait_TrA2VectorDictConstrain2)

  @test TrA2VectorDict <: traitsof(Vector{Int}, Dict{Int, String})
  if !(TrA2_2 <: traitsof[String])
    @test !(TrA2VectorDictConstrain2 <: traitsof(Vector{Int}, Dict{Int, String}))  # not working because String has no TrA2 trait
    traitsof[String] = TrA2_2
    traitsof_refixate()
  end
  if !(TrA2_1 <: traitsof[Int])
    @test !(TrA2VectorDictConstrain2 <: traitsof(Vector{Int}, Dict{Int, String}))  # still not working, String has no trait TrA2_1
    traitsof[Int] = TrA2_1
    traitsof_refixate()
  end
  @test TrA2VectorDictConstrain2 <: traitsof(Vector{Int}, Dict{Int, String})  # works, as both Traits constraints are fulfilled
  @test !(TrA2VectorDictConstrain2 <: traitsof(Vector{String}, Dict{Int, String}))  # String does not have TrA2_1

  @testset "unionall constraints" begin
    traitsof[Vector{<:Number} , Dict{T, <:AbstractString} where T] = TrA2VectorDict_fancy
    push!(traitsof, trait_TrA2VectorDictConstrain2_fancy)
    traitsof_refixate()

    @test TrA2VectorDict_fancy <: traitsof(Vector{Int}, Dict{Int, String})
    @test !(TrA2VectorDict_fancy <: traitsof(Vector{String}, Dict{Int, String}))
    @test !(TrA2VectorDict_fancy <: traitsof(Vector{Int}, Dict{Int, Bool}))

    @test TrA2VectorDictConstrain2_fancy <: traitsof(Vector{Int}, Dict{Int, String})
    if !(TrA2_1 <: traitsof(Bool))
      @test !(TrA2VectorDictConstrain2_fancy <: traitsof(Vector{Bool}, Dict{Int, String}))  # we already defined TrA2_3 for all Integer
      traitsof[Integer] = TrA2_1
      traitsof_refixate()
    end
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

trait_TrA3VectorDictDictConstrain3(::Traitsof, _...) = Union{}
function trait_TrA3VectorDictDictConstrain3(traitsof::Traitsof, ::Type{Vector{VE}}, ::Type{Dict{D1K, D1V}}, ::Type{Dict{D2K, D2V}}) where VE where D1K where D1V where D2K where D2V
  if TrA3_1 <: traitsof(VE) && TrA3_2 <: traitsof(D1V) && TrA3_3 <: traitsof(D2K)
    TrA3VectorDictDictConstrain3
  else
    Union{}
  end
end

trait_TrA3VectorDictDictConstrain3_fancy(::Traitsof, _...) = Union{}
function trait_TrA3VectorDictDictConstrain3_fancy(traitsof::Traitsof, ::Type{Vector{VE}}, ::Type{Dict{D1K, D1V}}, ::Type{Dict{D2K, D2V}}
  ) where VE where D1K where D1V<:AbstractString where D2K<:Number where D2V
  if TrA3_1 <: traitsof(VE) && TrA3_2 <: traitsof(D1V) && TrA3_3 <: traitsof(D2K)
    TrA3VectorDictDictConstrain3_fancy
  else
    Union{}
 end
end

@testset "arity3 with parametertypes" begin
  traitsof_refixate()

  traitsof[Vector, Dict, Dict] = TrA3VectorDictDict
  push!(traitsof, trait_TrA3VectorDictDictConstrain3)

  @test TrA3VectorDictDict <: traitsof(Vector{Int}, Dict{Int, String}, Dict{Int, Int})
  if !(TrA3_2 <: traitsof[String])
    @test !(TrA3VectorDictDictConstrain3 <: traitsof(Vector{Int}, Dict{Int, String}, Dict{Int, Int}))  # not working because String has no TrA3 trait
    traitsof[Int] = TrA3_1
    traitsof[String] = TrA3_2
    traitsof[Integer] = TrA3_3
    traitsof_refixate()
  end
  @test TrA3VectorDictDictConstrain3 <: traitsof(Vector{Int}, Dict{Int, String}, Dict{Int, Int})  # works, as both Traits constraints are fulfilled
  traitsof[AbstractString] = TrA3_1
  traitsof_refixate()
  @test TrA3VectorDictDictConstrain3 <: traitsof(Vector{String}, Dict{Int, String}, Dict{Bool, Int})  # still works as Bool is also a Number


  @testset "unionall constraints" begin
    traitsof[Vector{<:Number} , Dict{T, <:AbstractString} where T, Dict{<:Number}] = TrA3VectorDictDict_fancy
    push!(traitsof, trait_TrA3VectorDictDictConstrain3_fancy)
    traitsof_refixate()

    @test TrA3VectorDictDict_fancy <: traitsof(Vector{Int}, Dict{Int, String}, Dict{Int, Int})
    @test !(TrA3VectorDictDict_fancy <: traitsof(Vector{String}, Dict{Int, String}, Dict{Int, Int}))
    @test !(TrA3VectorDictDict_fancy <: traitsof(Vector{Int}, Dict{Int, Bool}, Dict{Int, Int}))

    @test TrA3VectorDictDictConstrain3_fancy <: traitsof(Vector{Int}, Dict{Int, String}, Dict{Int, Int})
    @test TrA3VectorDictDictConstrain3_fancy <: traitsof(Vector{Int}, Dict{Int, String}, Dict{Bool, Int})  # we already defined TrA3_3 for all Integer
    if !(TrA3_3 <: traitsof[Float32])
      @test !(TrA3VectorDictDictConstrain3_fancy <: traitsof(Vector{Int}, Dict{Int, String}, Dict{Float32, Int}))
      traitsof[AbstractFloat] = TrA3_3
      traitsof_refixate()
    end
    @test TrA3VectorDictDictConstrain3_fancy <: traitsof(Vector{Int}, Dict{Int, String}, Dict{Float32, Int})
  end
end
