module Traits
export @traits, @traits_show_implementation, isdef

include("Syntax/Syntax.jl")
using .Syntax

include("BasicTraits.jl")


# in addition to the core syntax we offer a shortcut to an implementation detail
# of julia which sooner or later will get officially supported for sure

"""
  checks whether the function is defined for the actual types or not

This works in compile time and hence can be used to optimize code.

CAUTION: may lead to unepxected results when queried with AbstractTypes
Note that you can always overwrite this function for your specific types.
"""
isdef(f, types::Type{<:Tuple}) = Core.Compiler.return_type(f, types) !== Union{}
isdef(f, types...) = isdef(f, Tuple{types...})

end # module
