module BasicTraits
export ismutable, isimmutable, isiterable, iscallable,
  isbitstype, isconcretetype

# mimicking typeclasses from SimpleTraits.BaseTraits

ismutable(T::DataType) = T.mutable
ismutable(::Type) = false
ismutable(value) = !Base.isimmutable(value)
Base.isimmutable(T::Type) = !ismutable(T)

const isiterable = Base.isiterable

# if we would like to test whether the type itself is callable, we would use ``iscallable(f) = !isempty(methods(f))``
# https://stackoverflow.com/questions/41658692/how-to-determine-if-julia-object-is-callable
# but Types are always callable, so this makes no sense here
function iscallable(T)
  if isdefined(T, :name) && isdefined(T.name, :mt)
    !isempty(T.name.mt)
  else
    false
  end
end

const isbitstype = Base.isbitstype
const isconcretetype = Base.isconcretetype

end  # module
