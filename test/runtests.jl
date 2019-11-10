using Traits
using Test

@testset "Utils" begin
  include("utils.jl")
end

e = :(
  function f{T}(a::A, b = 3, c::Int; kw = "hallo") where {T, A<:AbstractArray, isInt(A), companion(A) <: Number, isabstracttype(A), Base.IteratorSize(A) :: Base.HasShape, eltype(A) isa Int}
    42
  end
)
Traits._traits(Main,  e)

e2 = :(
  function f{T}(a::A, b; kwargs...) where {T, A}
    @match(a, isInt(A), companion(A), isabstracttype(A), Base.IteratorSize(A), eltype(A) isa Int; kwargs...) do f
      function f(a, ::BetweenArgsAndTypeVars, T, A, ::BetweenTypeVarsAndTraits, ::Type{<: Number}, ::Val{true}, ::Base.HasShape, ::Val{true}; kwargs ausgeschrieben)
        42
      end
    end
  end
)

Traits._traits(Main, e)
