using WhereTraits
using Test

using Suppressor
using StringDistances  # TODO make this a submodule for test only
using InteractiveUtils

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

# julia 1.2?
# function find_definition(out)
#   s = findfirst("Function", out)
#   out[s.start: end]
# end

# string distance should be small
@test evaluate(Levenshtein(), out1, out2) <= 10
# println(out1)
# println(out2)
@code_warntype fn(5)
