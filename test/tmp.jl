using Traits
using Traits.Utils
using Test
using Suppressor

# Test merging of several methods
# ===============================
methods
struct Tr1 end
struct Tr2 end
struct Tr3 end
struct Tr4 end

mytraitsof1(_) = Tr1
mytraitsof1(::Type{T} where T<:Number) = Tr2
mytraitsof1(::Type{Int}) = Tr3
mytraitsof1(::Type{Float16}) = Tr4

mytraitsof(Int)
mytraitsof(Float32)
mytraitsof(Float16)

# Test code_llvm
# ==============

fn(x::Integer) = 1

struct TrA end
fn(x::X) where X = fn(x, mytraitsof(X))
fn(x::AbstractFloat, ::TypeLB(TrA)) = 2
fn(x::AbstractFloat, _) = 3

mytraitsof1(::Type{Float32}) = TrA
mytraitsof1(::Type{Int}) = TrA

@test fn(5) == 1 # -> 1; dispatch only happens on the type
@test fn(Float32(5)) == 2 # -> 2; dispatch through traits
@test fn(Float64(5)) == 3 # -> 3; default dispatch through traits

struct TrB end
mytraitsof1(::Type{Float16}) = TrB
fn(x::AbstractFloat, ::TypeLB(TrB)) = 4

@test fn(Float16(5)) == 4 # -> 4; dispatch through traits
@test fn(Float32(5)) == 2 # -> 2; NO MethodError; nothing is overwritten, everything works like you would hope for


out1 = @capture_out begin
  @code_llvm fn(5)
end
out2 = @capture_out begin
  @code_llvm fn(Float16(5))
end

println(out1)
println(out2)  # unfortunately, this is not optimal code




helper(cont, range) = for i ∈ range
  cont(i)
end


function g()
  a = Ref(0)
  helper(1:4) do i
    a.x += i
  end
  a.x
end


function f()
  a = 0
  helper(1:4) do i
    a += i
  end
  a
end


@code_llvm g()

@code_llvm f()
