module Utils
using Reexport

include("UtilsGenerics.jl")
@reexport using .UtilsGenerics

include("UtilsExprs/UtilsExprs.jl")
@reexport using .UtilsExprs

include("Normalize/Normalize.jl")  # depends on Counter
@reexport using .Normalize

include("DocsHelpers.jl")
@reexport using .DocsHelpers

include("BoolTypes.jl")
@reexport using .BoolTypes

include("Counters.jl")
@reexport using .Counters

include("Ambiguities.jl")  # depends on Counter
@reexport using .Ambiguities

end  # module
