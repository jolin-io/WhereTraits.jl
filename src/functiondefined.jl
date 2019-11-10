function functiondefined end
const isdef = functiondefined


functiondefined(f, types) = return_type_reliable(types) ? functiondefined_abstract(f, types) : functiondefined_concrete(f, types)
functiondefined_concrete(f, types) = Core.Compiler.return_type(f, types) !== Union{}
function functiondefined_abstract(f, types)
  combinations = Iterators.product(leaftypes.(types.parameters)...)
  # empty list means Any (everything else should be inferrable)
  !isnothing(iterate(combinations)) && all(combinations) do leaf
    functiondefined_concrete(f, Tuple{leaf...})
  end
end

function return_type_reliable(types)
  # assuming Tuple type
  all(types.parameters) do t
    isconcretetype(t) # || isabstracttype(t)  # also abstractypes have errors in that they are assumed to be castable to the concrete Type
  end
end


"""
  leaftypes(Type)::[ConcreteTypes]

return list of representable concrete types

This follows the logic that if something is defined for Functor{} (without typeparameters)
  it is also defined for Functor{NewType}.
  And as there is no way to dispatch on Functor{NewType}, as NewType is completely new,
  we can conclude that if Functor{NewType} typeinfers, than Functor{} typeinfers.
Hence we can represent the abstract Functor{} with the concrete Functor{NewType}.

A similar reasoning can be done for plain type Any. Hence we map Any to NewType
"""
leaftypes(T::Union{UnionAll, DataType}) = isabstracttype(T) ? leaftypes_abstract(T) : leaftypes_concrete(T)

leaftypes_abstract(T::Union{UnionAll, DataType}) = vcat(leaftypes.(InteractiveUtils.subtypes(T))...)

# ignore NewTypes, which will also show up on inheritance list
leaftypes_concrete(T::DataType) = isnewtype(T) ? [] : [T]

function leaftypes_concrete(T::UnionAll)
  # we currently already deal with upperbounds (covariant type logic)
  # TODO support lowerbounds? (i.e. contravariant types)
  typevar2ub = collect_upper_bounds(T)
  T_unwrap = Base.unwrap_unionall(T)

  T_internal = T_unwrap.name
  # both following versions to access normalized parametric type lead to unreachable reached:
  constructor = getfield(T_internal.module, T_internal.name)
  newparameters = [p isa TypeVar ? newtype(typevar2ub[p]) : p for p in T_unwrap.parameters]
  [constructor{newparameters...}]
end

# extra handling of Any
leaftypes(::Type{Any}) = [newtype()]


"""
  newtype_create()
  newtype_create(UpperBound)

Create a newtype with the given UpperBound (defaults to Any as usual) using ``eval``.

This won't have problems with world age as world age only applies to functions and not types.
This also won't have problems with precompilation, as this function should only be called at runtime.

[One alternative guess would be to use @generated functions however
this will throw the error ``ERROR: syntax: "struct" expression not at top level``
Another alternative is to use standard dispatch, however here new constraints have to be added manually]
"""
# TODO it is forbidden to call ``eval`` in a generated function, hence this newtype does not work
# TODO that only appears sometimes, and not when calling ``traitsof(Dict)`` for instance
# we need to use where statement so that the eval will be called not within the generated function but outside
function newtype(::Type{UpperBound}) where UpperBound
# function newtype(UpperBound)
  sym = Symbol("##NewType$UpperBound")
  # we have only one NewType per UpperBound to prevent leakage in subtypes performance
  if isdefined(Utils, sym)
    getfield(Utils, sym)
  else
    Base.eval(Utils, quote
      struct $sym <: $UpperBound end
      $sym
    end)
  end
end

newtype() = newtype(Any)

# newtype(::Type{Any}) = newtype_create(Any)
# newtype()
# struct NewTypeReal <: Real end  # UpperBound in Complex
# newtype(::Type{Real}) = newtype_create()
# struct NewTypeInteger <: Integer end  # UpperBound in Rational
# newtype(::Type{Integer}) = NewTypeInteger


isnewtype(T::DataType) = startswith(string(T.name.name), "##NewType")

collect_upper_bounds(T) = Dict()
collect_upper_bounds(T::UnionAll) = Dict(T.var => T.var.ub, collect_upper_bounds(T.body)...)
