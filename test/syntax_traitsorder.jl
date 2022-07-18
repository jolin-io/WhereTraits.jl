using Test
using WhereTraits
using DataTypesBasic

# plain test
# ==========

@traits conflict(a, b) where {eltype(a) <: Number} = 1
@traits conflict(a, b) where {eltype(b) <: AbstractArray} = 2

@test conflict([1,2], "hi") == 1
@test conflict("other", [[]]) == 2

@test_throws WhereTraitsAmbiguityError conflict([1], [[2,3], []])

@traits_order conflict(a1, a2) begin
        eltype(a1)
        eltype(a2)
end

@test conflict([1], [[2,3], []]) == 1

@traits conflict(a, b) where {eltype(a) <: Number, eltype(b) <: AbstractArray} = 3

@test conflict([1], [[2,3], []]) == 3


# TODO complex test where the recursion of the `resolve` method is important


# module Test
# ===========

module MA
    using WhereTraits
    @traits function mymethod(a, b::T) where {T<:AbstractArray, isodd(a), Base.IteratorSize(T)::Base.HasShape}
        return "a = $a, size(b) = $(size(b))"
    end
end

module MB
using WhereTraits
    using ..MA
    @traits function MA.mymethod(a, b::B) where {B <: AbstractArray, Base.IteratorEltype(B)::Base.HasEltype}
        return "eltype(b) = $(eltype(b))"
    end
end

module MC
    using ..MA
    using ..MB  # not needed here, but in some different place it would be needed
    using DataTypesBasic
    using WhereTraits
    
    result1 = @Try MA.mymethod(1, [1,2,3])
    
    @traits_order (Main.MA).mymethod(a1, a2::T2) where {T2 <: AbstractArray} begin
        Base.IteratorEltype(T2)
        isodd(a1)
        Base.IteratorSize(T2)
    end
    
    result2 = @Try MA.mymethod(1, [1,2,3])
end

@test isa(MC.result1, Const{<:Thrown{WhereTraitsAmbiguityError}})
@test MC.result1.value.exception.traits_conflicting == [
    :(isodd(a1)),
    :(Base.IteratorSize(T2)),
    :(Base.IteratorEltype(T2)),
]

@test isa(MC.result2, Identity)
@test MC.result2.value == "eltype(b) = $(eltype([1,2,3]))"