using Traits.BasicTraits
@test Traits.traitsof(Int) == Union{typeof(iterate), BitsType, Immutable}
@test Traits.traitsof(typeof(+)) == Union{BitsType, Callable, Immutable}

struct MyImmutableType end
@test Traits.traitsof(MyImmutableType) == Union{BitsType, Immutable}  # BitsType!!

mutable struct MyMutableType end
@test Traits.traitsof(MyMutableType) == Union{NoBitsType, Mutable}  # NoBitsType!!
