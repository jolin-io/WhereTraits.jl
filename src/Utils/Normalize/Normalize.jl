module Normalize
export normalize_mod_and_name, normalized_arg_by_position

using Reexport

include("NormalizeType.jl")
@reexport using .NormalizeType

using SimpleMatch: @match
using ExprParsers


normalized_arg_by_position(position::Int) = Symbol("a", position)



function normalize_mod_and_name(mod, name)
    parser = EP.AnyOf(EP.anysymbol, EP.NestedDot())
    normalize_mod_and_name(mod, parse_expr(parser, name))
end

normalize_mod_and_name(mod, name::Symbol) = mod, name

function normalize_mod_and_name(mod, name::EP.NestedDot_Parsed)
    mod′::Module = getproperty(mod, name.base)
    for field in name.properties[1:end-1]
        mod′ = getproperty(mod′, field)
    end
    mod′, name.properties[end]  # NestedDot.properties is known to be non-empty
end


function unique_funcname(mod, funcname)
    mod′, funcname′ = normalize_mod_and_name(mod, funcname)
    Symbol(mod′, :., funcname′)
end


end  # module