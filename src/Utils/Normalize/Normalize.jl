module Normalize
export isdefined_generalized, normalize_mod_and_name, normalized_arg_by_position

using Reexport

include("NormalizeType.jl")
@reexport using .NormalizeType

using SimpleMatch: @match
using ExprParsers


normalized_arg_by_position(position::Int) = Symbol("a", position)

"""
    isdefined_generalized(mod, name)

like `Base.isdefined`, however can handle nested, qualified names (i.e. containing dots)

Examples
--------

```jldoctest
julia> using WhereTraits.Utils: isdefined_generalized

julia> isdefined_generalized(Main, :(Base.IteratorSize))
true

julia> isdefined_generalized(Main, :(Base.HasShape2))
false
```
"""
function isdefined_generalized(mod, name)
    try
        mod, name = normalize_mod_and_name(mod, name)
    catch ex
        isa(ex, UndefVarError) || rethrow()
        return false
    end
    isdefined(mod, name)
end


"""
    normalize_mod_and_name(mod, name)

Does nothing if `name` is a Symbol.
If `name` is a qualified name (i.e. containing a reference to a submodule),
the final submodule and simple Symbol name are returned.

Examples
--------
```jldoctest
julia> using WhereTraits.Utils: normalize_mod_and_name

julia> normalize_mod_and_name(Base, :iseven)
(Base, :iseven)

julia> normalize_mod_and_name(Base, :(Iterators.CountFrom))
(Base.Iterators, :CountFrom)
```
"""
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