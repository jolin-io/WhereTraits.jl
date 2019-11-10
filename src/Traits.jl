module Traits
export @traits, @traits_show_implementation, functiondefined, isdef

include("Syntax/Syntax.jl")
using .Syntax
include("BasicTraits.jl")
include("functiondefined.jl")

end # module
