module Traits
export traitsof, TypeLowerBound, TypeLB, Constrain1, Constrain2, Constrain3
# Traits.functions needs to be accessed qualified, i.e. Traits.functions
include("Utils.jl")
using Base.Iterators
using .Utils

# TODO adapt doc
"""
    traitsof(type)  # query current generated traits for ``type``
    traitsof[type]  # == traitsof(type)
    traitsof[type] = TraitType  # add new ``TraitType`` as a trait to ``type``

    Traits.fix_traits()  # regenerate Traits from internal dictionaries

Return the trait types of a given Type ``type``. If the type has multiple traits, a Union Type is returned.

# Examples
```jldoctest
julia> traitsof[Integer]  # default value
Union{}
julia> traitsof[Integer] = Union{Integer, String}
julia> Traits.fix_traits()  # needed to regenerate Traits
julia> traitsof[Integer]
Union{Integer, String}
julia> traitsof(Integer)  # [] or () - both do the same
Union{Integer, String}
julia> struct MyType end
julia> traitsof(MyType)  # everything has Union{} as default value
Union{}
```
"""
function traitsof end

"""
support complex dependencies on typeparameters for a single type
"""
struct Constrain1{T}
  type::Type{T}
  parameter_constraints::Tuple

  function Constrain1{T}(parameter_constraints...) where T
    if length(Base.unwrap_unionall(T).parameters) != length(parameter_constraints)
      error("need exactly as many parameter trait specifications as type ``$T`` has parameters")
    end
    new{T}(T, parameter_constraints)
  end
end

"""
support complex dependencies on typeparameters for two types
"""
struct Constrain2{T1, T2}
  type::Type{Tuple{T1, T2}}
  parameter_constraints::Tuple

  function Constrain2{T1, T2}(parameter_constraints...) where T1 where T2
    if sum(length(Base.unwrap_unionall(t).parameters) for t in (T1, T2)) != length(parameter_constraints)
      error("need exactly as many parameter trait specifications as types ``$T1, $T2`` have parameters in total")
    end
    new{T1, T2}(Tuple{T1, T2}, parameter_constraints)
  end
end

"""
support complex dependencies on typeparameters for three types
"""
struct Constrain3{T1, T2, T3}
  types::Type{Tuple{T1, T2, T3}}
  parameter_constraints::Tuple

  function Constrain3{T1, T2, T3}(parameter_constraints...) where T1 where T2 where T3
    if sum(length(Base.unwrap_unionall(t).parameters) for t in (T1, T2, T3)) != length(parameter_constraints)
      error("need exactly as many parameter trait specifications as types ``$T1, $T2, $T3`` have parameters in total")
    end
    new{T1, T2, T3}(Tuple{T1, T2, T3}, parameter_constraints)
  end
end

# one dict for each arity to increase search speed
# Think of this like a partitioning over arity
# we cannot easily partition by number of typeparameters as parent types without parameters
# should still match and hence would have to be searched for anyway
dict_traitsof1 = Dict{Union{Type, Constrain1}, Type}()
dict_traitsof2 = Dict{Union{Type{Tuple{T1, T2}} where T1 where T2, Constrain2}, Type}()
dict_traitsof3 = Dict{Union{Type{Tuple{T1, T2, T3}} where T1 where T2 where T3, Constrain3}, Type}()


Base.getindex(::typeof(traitsof), args...) = traitsof(args...)

function Base.setindex!(::typeof(traitsof), ::Type{Value}, ::Type{Key}) where Value where Key
  dict_traitsof1[Key] = Union{Value, get(dict_traitsof1, Key, Union{})}
end
function Base.setindex!(::typeof(traitsof), ::Type{Value}, c1::Constrain1) where Value where Key
  dict_traitsof1[c1] = Union{Value, get(dict_traitsof1, c1, Union{})}
end

function Base.setindex!(::typeof(traitsof), ::Type{Value}, ::Type{Key1}, ::Type{Key2}) where Value where Key1 where Key2
  dict_traitsof2[Tuple{Key1, Key2}] = Union{Value, get(dict_traitsof2, Tuple{Key1, Key2}, Union{})}
end
function Base.setindex!(::typeof(traitsof), ::Type{Value}, c2::Constrain2) where Value where Key1 where Key2
  dict_traitsof2[c2] = Union{Value, get(dict_traitsof2, c2, Union{})}
end

function Base.setindex!(::typeof(traitsof), ::Type{Value}, ::Type{Key1}, ::Type{Key2}, ::Type{Key3}) where Value where Key1 where Key2 where Key3
  dict_traitsof3[Tuple{Key1, Key2, Key3}] = Union{Value, get(dict_traitsof3, Tuple{Key1, Key2, Key3}, Union{})}
end
function Base.setindex!(::typeof(traitsof), ::Type{Value}, c3::Constrain3) where Value where Key1 where Key2
  dict_traitsof3[c3] = Union{Value, get(dict_traitsof3, c3, Union{})}
end



fix_traits() = @eval begin
  @generated function traitsof(::Type{T}) where T
    u = Union{}
    for (key, value) in dict_traitsof1
      if key isa Constrain1
        c1::Constrain1 = key
        if T <: c1.type && all(traitsof(p) <: typeLB for (p, typeLB) ∈ zip(T.parameters, c1.parameter_constraints))
          u = Union{u, value}
        end
      else
        if T <: key
          u = Union{u, value}
        end
      end
    end
    :($u)
  end


  @generated function traitsof(::Type{T1}, ::Type{T2}) where T1 where T2
    u = Union{}
    for (key, value) in dict_traitsof2
      if key isa Constrain2
        c2::Constrain2 = key
        if Tuple{T1, T2} <: c2.type && all(traitsof(p) <: typeLB for (p, typeLB) ∈ zip(chain(T1.parameters, T2.parameters), c2.parameter_constraints))
          u = Union{u, value}
        end
      else
        if Tuple{T1, T2} <: key
          u = Union{u, value}
        end
      end
    end
    :($u)
  end


  @generated function traitsof(::Type{T1}, ::Type{T2}, ::Type{T3}) where T1 where T2 where T3
    u = Union{}
    for (key, value) in dict_traitsof3
      if key isa Constrain3
        c3::Constrain3 = key
        if Tuple{T1, T2, T3} <: c3.type && all(traitsof(p) <: typeLB for (p, typeLB) ∈ zip(chain(T1.parameters, T2.parameters, T3.parameters), c3.parameter_constraints))
          u = Union{u, value}
        end
      else
        if Tuple{T1, T2, T3} <: key
          u = Union{u, value}
        end
      end
    end
    :($u)
  end

end

fix_traits()


"""
    TypeLowerBound(type_lower_bound) = Type{T} where type_lower_bound <: T <: Any

short ``TypeLB``

# Examples
```jldoctest
julia> TypeLB(Union{Integer, String})
Type{T} where T>:Union{Integer, String}
```
"""
TypeLowerBound(::Type{LB}) where LB = Type{>:LB}
TypeLB = TypeLowerBound



"""
    functions(trait_type)

Returns all functions which are associated with the given ``trait_type`` Type.

This is just for convention, i.e. overload it for your custom Trait
so that your Users know which functions they are supposed to implement.

For Traits which are defined as Unions of Function types, the functions are extracted automatically.

# Examples
```jldoctest
julia> functions(Union{typeof(+), typeof(identity)})
2-element Array{Function,1}:
 +
 identity
```
"""
function functions end
functions(::Type{T}) where T = []
functions(u::Union) = [functions(u.a); functions(u.b)]
functions(f::Type{T} where T <: Function) = _functions_isfunction(f)
# if a Union contains only functions, it is also a subtype of Function
_functions_isfunction(u::Union) = [functions(u.a); functions(u.b)]
_functions_isfunction(f) = [f.instance]  # ``fieldnames`` don't work here, but ``propertynames`` do to see the fields of typeof(+) e.g.


end # module
