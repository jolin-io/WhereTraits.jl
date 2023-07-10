module WhereTraits
export @traits, @traits_order, @traits_test, @traits_store, @traits_show_implementation
export WhereTraitsException, WhereTraitsAmbiguityError, WhereTraitsMethodError

Base.@kwdef mutable struct _Config
    auto_documentation::Bool = true
end
# TODO documentation of struct fields does not seem to work - hence we document the constant instead

const CONFIG = _Config()

include("Utils/Utils.jl")
using .Utils

# this syntax is so complex that we need to store a state for each function
# which because of its complexity is defined in an extra submodule
include("InternalState.jl")
using .InternalState

include("Syntax/Syntax.jl")
using .Syntax

include("ExtraHelpers.jl")
using .ExtraHelpers

include("BasicTraits.jl")

end # module
