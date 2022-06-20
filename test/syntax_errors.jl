using WhereTraits
using Test


# DONE TODO in case a trait is illegally defined (e.g. referring variable `b` while there is no b, every call to the traits functions fails). There should be an immediate error in best case.

@testset "UndefVarError" begin

    @traits f(a) where {iseven(a)} = :works
    @test f(2) == :works

    @test_throws UndefVarError(:iseventypo) @traits f(a) where {iseventypo(a)} = 2a
    @test_throws UndefVarError(:b) @traits f(a) where {isodd(b)} = 2a

    c = 5
    # raises a warning which should be testable with @test_logs, but I could not make it work
    @traits g(a) where {isodd(c)} = :workstoo
    @test g(1) == :workstoo

    @test_throws UndefVarError(:HasShape2) @traits h(a) where {Base.IteratorSize(a)::Base.HasShape2} = 2

end


@testset "WhereTraitsMethodError" begin

    @traits conflict(a) where {Base.IteratorSize(a) <: Base.HasShape} = 2
    @test_throws WhereTraitsMethodError conflict([1])  # TraitsMethodError

    @traits conflict2(a) where {Base.IteratorSize(a)::Base.HasShape} = 2
    @test conflict2([1]) == 2
    @test_throws WhereTraitsMethodError conflict2(Iterators.countfrom()) # TraitsMethodError
    
end
