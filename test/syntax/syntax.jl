using Traits
using Test

@testset "Utils" begin
  include("utils.jl")
end

e = :(
  function f{T}(a::A, b = 3, c::Int; kw = "hallo") where {T, A<:AbstractArray, isInt(A), companion(A) <: Number, !isabstracttype(A), Base.IteratorSize(A) :: Base.HasShape, eltype(A) isa Int}
    42
  end
)
# rough goal (we currently support even more detailed syntax)
e2 = quote
  function f{T}(a::A, b; kwargs...) where {T, A}
    f_traits(a, isInt(A), companion(A), isabstracttype(A), Base.IteratorSize(A), eltype(A) isa Int; kwargs...)
  end
  function f_traits(a, ::BetweenArgsAndTypeVars, T, A, ::BetweenTypeVarsAndTraits, ::Type{<:Number}, ::Val{true}, ::Base.HasShape, ::Val{true}; kw = "hallo")
    42
  end
end


# TODO test parsing in as many cases as possible
Traits.Syntax._traits(Main,  e)
