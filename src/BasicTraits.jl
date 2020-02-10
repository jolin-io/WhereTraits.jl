module BasicTraits
export @overwrite_Base, ismutable, isimmutable, isiterable, iscallable,
  isbitstype, isconcretetype

macro overwrite_Base()
  esc(quote
    using Traits.BasicTraits
    const isimmutable = Traits.BasicTraits.isimmutable
    const isbitstype = Traits.BasicTraits.isbitstype
    const isconcretetype = Traits.BasicTraits.isconcretetype
    nothing  # empty return
  end)
end
# mimicking typeclasses from SimpleTraits.BaseTraits

ismutable(T::DataType) = T.mutable
ismutable(::Type) = false
ismutable(value) = ismutable(typeof(value))

# isimmutable is already part of Base export, hence we better use it,
# however it says "Note that this function works on values"
# i.e. we could overwrite the Base.isimmutable for Type, however this is Type piracy and potentially dangerous
# hence we define our own isimmutable
isimmutable(any) = !ismutable(any)

isiterable(T::Type) = Base.isiterable(T)
isiterable(any) = isiterable(typeof(any))


"""
this checks whether there are functions defined on a given TYPE

i.e. use it like ``iscallable(typeof(+))``

for convenience ``iscallable(value) = iscallable(typeof(value))``
"""
@generated function iscallable(::Type{T}) where T
  # without ``@generated`` it fails to type infer properly
  lim = -1
  world = typemax(UInt)
  methods = Base._methods_by_ftype(Tuple{T, Vararg}, lim, world)
  !isempty(methods)
end
iscallable(value) = iscallable(typeof(value))

isbitstype(T::Type) = Base.isbitstype(T)
isbitstype(value) = isbitstype(typeof(value))

isconcretetype(T::Type) = Base.isconcretetype(T)
isconcretetype(value) = isconcretetype(typeof(value))


end  # module
