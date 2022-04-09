module BasicTraits
export ismutable, isimmutable, isiterable, iscallable,
  isbitstype, isconcretetype

"""
    WhereTraits.BasicTraits.@overwrite_Base

Overloading functions already defined in base, so that they work similar to `Base.eltype`
in that they are best defined on Types, with a fallback from values which just grabs the type of the value
and tries it again.

The following functions are currently over-defined:
- isimmutable
- ismutable
- isbitstype
- isconcretetype

In addition, also `using WhereTraits.BasicTraits` is run.
"""
macro overwrite_Base()
  esc(quote
    using WhereTraits.BasicTraits
    const isimmutable = WhereTraits.BasicTraits.isimmutable
    const ismutable = WhereTraits.BasicTraits.ismutable
    const isbitstype = WhereTraits.BasicTraits.isbitstype
    const isconcretetype = WhereTraits.BasicTraits.isconcretetype
    nothing  # empty return
  end)
end

# mimicking typeclasses from SimpleTraits.BaseTraits
# --------------------------------------------------

"""
    ismutable(v) -> Bool

Return `true` iff value `v` is mutable.  Note that this function works on Types instead of values
(oppositely to `Base.isimmutable`). When applied to a value, a default clause will match on its type instead.

# Examples
```jldoctest
julia> using WhereTraits; WhereTraits.BasicTraits.@overwrite_Base

julia> ismutable(1)
false

julia> ismutable([1,2])
true
```
"""
function ismutable end

if VERSION >= v"1.7.0"
  function ismutable(t::Type)
    # adapted from julia 1.7 Base.ismutabletype
    t = Base.unwrap_unionall(t)
    # TODO: what to do for `Union`?
    return isa(t, DataType) && t.name.flags & 0x2 == 0x2
  end
else
  function ismutable(t::Type)
    # adapted from julia 1.7 Base.ismutabletype
    t = Base.unwrap_unionall(t)
    # TODO: what to do for `Union`?
    return isa(t, DataType) && t.mutable
  end
end

ismutable(value) = ismutable(typeof(value))

# isimmutable is already part of Base export, hence we better use it,
# however it says "Note that this function works on values"
# i.e. we could overwrite the Base.isimmutable for Type, however this is Type piracy and potentially dangerous
# hence we define our own isimmutable
"""
    isimmutable(v) -> Bool

Return `true` iff value `v` is immutable.  Note that this function works on Types instead of values
(oppositely to `Base.isimmutable`). When applied to a value, a default clause will match on its type instead.

# Examples
```jldoctest
julia> using WhereTraits; WhereTraits.BasicTraits.@overwrite_Base

julia> isimmutable(1)
true

julia> isimmutable([1,2])
false
```
"""
isimmutable(any) = !ismutable(any)

"""
    isiterable(T) -> Bool

Test if type `T` is an iterable collection type or not,
that is whether it has an `iterate` method or not.

When given a value instead of a Type, it fallbacks to use `typeof(value)`.
"""
isiterable(T::Type) = Base.isiterable(T)
isiterable(any) = isiterable(typeof(any))


"""
    callable(T) -> Bool

Checks whether the call-syntax is defined for the given Type.
For convenience `iscallable(value) = iscallable(typeof(value))`

# Examples
```jldoctest
julia> using WhereTraits; WhereTraits.BasicTraits.@overwrite_Base

julia> iscallable(typeof(+))
true

julia> iscallable(+)
true

julia> iscallable(Some)
false

julia> iscallable(typeof(Some))
true
```
"""
@generated function iscallable(::Type{T}) where T
  # without `@generated` it fails to type infer properly
  lim = -1
  world = typemax(UInt)
  methods = Base._methods_by_ftype(Tuple{T, Vararg}, lim, world)
  !isempty(methods)
end
iscallable(value) = iscallable(typeof(value))

"""
    isbitstype(T)

Return `true` if type `T` is a "plain data" type,
meaning it is immutable and contains no references to other values,
only `primitive` types and other `isbitstype` types.
Typical examples are numeric types such as `UInt8`,
`Float64`, and `Complex{Float64}`.
This category of types is significant since they are valid as type parameters,
may not track `isdefined` / `isassigned` status,
and have a defined layout that is compatible with C.

For convenience `isbitstype(value) = isbitstype(typeof(value))`

# Examples
```jldoctest
julia> using WhereTraits; WhereTraits.BasicTraits.@overwrite_Base

julia> isbitstype(Complex{Float64})
true

julia> isbitstype(Complex)
false

julia> isbitstype(1)
true
```
"""
isbitstype(T::Type) = Base.isbitstype(T)
isbitstype(value) = isbitstype(typeof(value))


"""
    isconcretetype(T)

Determine whether type `T` is a concrete type, meaning it could have direct instances
(values `x` such that `typeof(x) === T`).

For convenience `isconcretetype(value) = isconcretetype(typeof(value))`

# Examples
```jldoctest
julia> using WhereTraits; WhereTraits.BasicTraits.@overwrite_Base

julia> isconcretetype(Complex)
false

julia> isconcretetype(Complex{Float32})
true

julia> isconcretetype(Vector{Complex})
true

julia> isconcretetype(Vector{Complex{Float32}})
true

julia> isconcretetype(Union{})
false

julia> isconcretetype(Union{Int,String})
false

julia> isconcretetype([1,2,3])
true

julia> isconcretetype("hi")
true
```
"""
isconcretetype(T::Type) = Base.isconcretetype(T)
isconcretetype(value) = isconcretetype(typeof(value))


end  # module
