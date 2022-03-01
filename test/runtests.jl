using WhereTraits
using Test
using Documenter

@test isempty(detect_ambiguities(WhereTraits))

@testset "doctest" begin
  doctest(WhereTraits)
end

@testset "syntax traits" begin
  include("syntax_traits.jl")
end

@testset "syntax traitsorder" begin
  include("syntax_traitsorder.jl")
end

@testset "README" begin
  include("README.jl")
end

@testset "BasicTraits" begin
  include("BasicTraits.jl")
end
