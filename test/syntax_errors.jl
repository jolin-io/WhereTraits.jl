using WhereTraits
using Test


# DONE in case a trait is illegally defined (e.g. referring variable `b` while there is no b, every call to the traits functions fails). There should be an immediate error in best case.

@testset "UndefVarError" begin

    @traits_test syntax_errors_f(a) where {iseven(a)} = :works
    @test syntax_errors_f(2) == :works

    @test_throws UndefVarError(:iseventypo) @traits_test syntax_errors_f(a) where {iseventypo(a)} = 2a
    @test_throws UndefVarError(:b) @traits_test syntax_errors_f(a) where {isodd(b)} = 2a

    # TODO this fails because c cannot be found in the definition of @traits_test.
    # @eval c = 5
    # # raises a warning which should be testable with @test_logs, but I could not make it work
    # @traits_test syntax_errors_g(a) where {isodd(c)} = :workstoo
    # @test syntax_errors_g(1) == :workstoo

    @test_throws UndefVarError(:HasShape2) @traits_test syntax_errors_h(a) where {Base.IteratorSize(a)::Base.HasShape2} = 2

end


@testset "WhereTraitsMethodError" begin

    @traits_test syntax_errors_conflict(a) where {Base.IteratorSize(a) <: Base.HasShape} = 2
    @test_throws WhereTraitsMethodError syntax_errors_conflict([1])  # TraitsMethodError

    @traits_test syntax_errors_conflict2(a) where {Base.IteratorSize(a)::Base.HasShape} = 2
    @test syntax_errors_conflict2([1]) == 2
    @test_throws WhereTraitsMethodError syntax_errors_conflict2(Iterators.countfrom()) # TraitsMethodError
    
end
