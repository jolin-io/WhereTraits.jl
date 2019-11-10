using Traits.BasicTraits
using Test

# we only test for subset relationship because traits might be added during this test run
@test isbitstype(Int)
@test isitable(Int)
@test isimmutable(Int)
@test isconcretetype(Int)

@test isbitstype(typeof(+))
@test iscallable(typeof(+))
@test isimmutable(typeof(+))
@test isconcretetype(typeof(+))

struct MyImmutableType end
@test isbitstype(MyImmutableType)
@test isimmutable(MyImmutableType)
@test isconcretetype(MyImmutableType)

mutable struct MyMutableType end
@test !isbitstype(MyMutableType)
@test ismutable(MyMutableType)
@test isconcretetype(MyMutableType)

@test !isconcretetype(Dict)
@test !isconcretetype(Any)
@test isconcretetype(Int)
