using Suppressor
using StringDistances  # TODO make this a submodule for test only

# Test README.md example
# ======================

fn(x::Integer) = 1

struct Tr end
fn(x::X) where X = fn(x, traitsof(X))
fn(x::AbstractFloat, ::TypeLB(Tr)) = 2
fn(x::AbstractFloat, _) = 3

traitsof[Float32] = Tr
traitsof[Int] = Tr

@test fn(5) == 1 # -> 1; dispatch only happens on the type
@test fn(Float32(5)) == 2 # -> 2; dispatch through traits
@test fn(Float64(5)) == 3 # -> 3; default dispatch through traits


struct Tr2 end
traitsof[Float16] = Tr2
fn(x::AbstractFloat, ::TypeLB(Tr2)) = 4

@test fn(Float16(5)) == 4 # -> 4; dispatch through traits
@test fn(Float32(5)) == 2 # -> 2; NO MethodError; nothing is overwritten, everything works like you would hope for


out1 = @capture_out begin
  @code_llvm fn(5)
end
out2 = @capture_out begin
  @code_llvm fn(Float16(5))
end

# string distance should be small
@test evaluate(Levenshtein(), "New York", "New Yorks") <= 10
