using Traits
using Test

@testset "syntax" begin
  include("syntax/syntax.jl")
end


@testset "README" begin
  include("README.jl")
end
