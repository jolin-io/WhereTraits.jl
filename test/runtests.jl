using WhereTraits
using Test
using Documenter

@test isempty(detect_ambiguities(WhereTraits))

@testset "doctest" begin
  doctest(WhereTraits)
end

@testset "syntax" begin
  include("syntax.jl")
end

@testset "README" begin
  include("README.jl")
end

@testset "BasicTraits" begin
  include("BasicTraits.jl")
end
