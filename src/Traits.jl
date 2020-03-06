module Traits
export @traits, @traits_test, @traits_show_implementation

@Base.kwdef mutable struct _Config
  # set to false because ``@suppress`` does currently not work with function definitions
  # for updates see https://github.com/JuliaIO/Suppressor.jl/issues/29
  suppress_on_traits_definitions::Bool = false
  auto_documentation::Bool = true
end
# TODO documentation of struct fields does not seem to work - hence we document the constant instead
"""
suppress_on_traits_definitions::Bool = false

    if true, all warnings, e.g. overwrite warnings are suppressed within @traits definitions

    this is useful, because in the current Julia, @traits is inherintly state-ful and edge-cases
    may need to overwrite previous definitions of the same @traits
"""
const CONFIG = _Config()

include("Utils/Utils.jl")
include("Syntax/Syntax.jl")
using .Syntax

include("BasicTraits.jl")

end # module
