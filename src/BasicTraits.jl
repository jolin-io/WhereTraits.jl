module BasicTraits
export basictraits, BitsType, NoBitsType, Immutable, Mutable, Callable
import ..Traitsof

# mimicking typeclasses from SimpleTraits.BaseTraits
struct BitsType end
struct NoBitsType end
struct Immutable end
struct Mutable end


# no opposite, as you want to check Callable in order to call something
# and there is no need to check for non-callable as you never want to not call something
struct Callable end

iscallabletype(T) = !isempty(T.name.mt)
# if we would like to test whether the type itself is callable, we would use ``iscallable(f) = !isempty(methods(f))``
# https://stackoverflow.com/questions/41658692/how-to-determine-if-julia-object-is-callable
# but Types are always callable, so this makes no sense here ;-)

## we don't need a trait for ConcreteType, as the types which are asked for traits have to be concrete always
# struct ConcreteType end

function basictraits(::Traitsof, value::Type)
  newtraits = Union{}
  if isbitstype(value)
    newtraits = Union{newtraits, BitsType}
  else
    newtraits = Union{newtraits, NoBitsType}
  end
  if value.mutable
    newtraits = Union{newtraits, Mutable}
  else
    newtraits = Union{newtraits, Immutable}
  end
  if iscallabletype(value)
    newtraits = Union{newtraits, Callable}
  end
  ## we don't need a trait for ConcreteType, as the types which are asked for traits have to be concrete always
  # if isconcretetype(value)
  #   newtraits = Union{newtraits, ConcreteType}
  # end
  if hasmethod(iterate, Tuple{value})
    newtraits = Union{newtraits, typeof(iterate)}
  end
  newtraits
end
end  # module
