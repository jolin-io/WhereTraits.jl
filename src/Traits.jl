module Traits
export @init_traitsof, TypeLowerBound, TypeLB, Constrain1, Constrain2, Constrain3

# Traits.functions needs to be accessed qualified, i.e. Traits.functions
include("Utils.jl")
include("BasicTraits.jl")
using .Utils

"""
support complex dependencies on typeparameters for a single type
"""
struct Constrain1{T}
  type::Type{T}
  parameter_constraints::Tuple

  function Constrain1{T}(parameter_constraints...) where T
    if length(parametersof(T)) != length(parameter_constraints)
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
    if sum(length(parametersof(t)) for t in (T1, T2)) != length(parameter_constraints)
      error("need exactly as many parameter trait specifications as types ``$T1, $T2`` have parameters in total")
    end
    new{T1, T2}(Tuple{T1, T2}, parameter_constraints)
  end
end

"""
support complex dependencies on typeparameters for three types
"""
struct Constrain3{T1, T2, T3}
  type::Type{Tuple{T1, T2, T3}}
  parameter_constraints::Tuple

  function Constrain3{T1, T2, T3}(parameter_constraints...) where T1 where T2 where T3
    if sum(length(parametersof(t)) for t in (T1, T2, T3)) != length(parameter_constraints)
      error("need exactly as many parameter trait specifications as types ``$T1, $T2, $T3`` have parameters in total")
    end
    new{T1, T2, T3}(Tuple{T1, T2, T3}, parameter_constraints)
  end
end

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
eval the definition of traitsof in the current module

This is mainly used to easily precompile traitsof.
Preventing type piracy is also targeted, but not so much, as traits can only be added to.
Still, by adding certain traits you can break others code.

With your own traitsof definition, such problems are all fixed.
"""
macro init_traitsof(other_traitsof_functions...)
  # because we use nested quotes the only way for hygiene seems to be to do it manual
  @gensym dict_traitsof1 dict_traitsof1_typeparams
  @gensym TypeTuple2 dict_traitsof2 dict_traitsof2_typeparams
  @gensym TypeTuple3 dict_traitsof3 dict_traitsof3_typeparams

  dict_traitsof1 = :dict_traitsof1
  dict_traitsof1_typeparams = :dict_traitsof1_typeparams
  dict_traitsof2 = :dict_traitsof2
  dict_traitsof2_typeparams = :dict_traitsof2_typeparams
  dict_traitsof3 = :dict_traitsof3
  dict_traitsof3_typeparams = :dict_traitsof3_typeparams

  esc(quote
    # escape this, as we want to define
    # the traitsof function in the calling environment

    # TODO adapt doc
    """
        traitsof(type)  # query current generated traits for ``type``
        traitsof[type]  # == traitsof(type)
        traitsof[type] = TraitType  # add new ``TraitType`` as a trait to ``type``

        refixate_traitsof()  # regenerate Traits from internal dictionaries

    Return the trait types of a given Type ``type``. If the type has multiple traits, a Union Type is returned.

    # Examples
    ```jldoctest
    julia> traitsof(Integer)  # default value
    Union{}
    julia> traitsof[Integer] = Union{Integer, String}
    julia> traitsof[Integer]
    julia> Union{}  # traitsof is generated function and hence need to be recompiled to change values
    julia> refixate_traitsof()  # with this
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

    # we partition the lookup for traits by 1) arity, and 2) typeparameters
    # where we create lookup tables for all respective types and unionall types

    # for types without typeparameters we can directly map the type to a respective collection of Traits
    $dict_traitsof1 = Dict{Type, Type}()
    # types with typeparameters are looked up in two stages
    # first as the generalized UnionAll to summarize all different types around that parametric type
    # second as a concrete mapping of types to traits
    $dict_traitsof1_typeparams = Dict{UnionAll, Dict{Union{Type, Traits.Constrain1}, Type}}()
    # same as for arity1
    # unfortunately it is not possible to construct a TupleType of UnionAll... hence we keep the generic TypeTuple2
    const $TypeTuple2 = Type{Tuple{T1, T2}} where T1 where T2
    $dict_traitsof2 = Dict{$TypeTuple2, Type}()
    $dict_traitsof2_typeparams = Dict{$TypeTuple2, Dict{Union{$TypeTuple2, Traits.Constrain2}, Type}}()
    # same as for arity2
    const $TypeTuple3 = Type{Tuple{T1, T2, T3}} where T1 where T2 where T3
    $dict_traitsof3 = Dict{$TypeTuple3, Type}()
    $dict_traitsof3_typeparams = Dict{$TypeTuple3, Dict{Union{$TypeTuple3, Traits.Constrain3}, Type}}()


    Base.getindex(::typeof(traitsof), args...) = traitsof(args...)

    function Base.setindex!(::typeof(traitsof), ::Type{Value}, ::Type{Key}) where Value where Key

      if Traits.hasnotypeparameters(Key)
        $dict_traitsof1[Key] = Union{Value, get($dict_traitsof1, Key, Union{})}
      else
        # TODO set empty subdict if necessary
        subdict = get!($dict_traitsof1_typeparams, Traits.normalize_parametrictype(Key), Dict{Union{Type, Traits.Constrain1}, Type}())
        subdict[Key] = Union{Value, get(subdict, Key, Union{})}
      end
    end
    function Base.setindex!(::typeof(traitsof), ::Type{Value}, c1::Traits.Constrain1{Key}) where Value where Key
      subdict = get!($dict_traitsof1_typeparams, Traits.normalize_parametrictype(Key), Dict{Union{Type, Traits.Constrain1}, Type}())
      subdict[c1] = Union{Value, get(subdict, c1, Union{})}
    end

    function Base.setindex!(::typeof(traitsof), ::Type{Value}, ::Type{Key1}, ::Type{Key2}) where Value where Key1 where Key2
      Key = Tuple{Key1, Key2}
      all_keys = (Key1, Key2)
      if all(Traits.hasnotypeparameters.(all_keys))
        $dict_traitsof2[Key] = Union{Value, get($dict_traitsof2, Key, Union{})}
      else
        Key_normalized = Tuple{Traits.normalize_parametrictype.(all_keys)...}
        subdict = get!($dict_traitsof2_typeparams, Key_normalized, Dict{Union{$TypeTuple2, Traits.Constrain2}, Type}())
        subdict[Key] = Union{Value, get(subdict, Key, Union{})}
      end
    end
    function Base.setindex!(::typeof(traitsof), ::Type{Value}, c2::Traits.Constrain2{Key1, Key2}) where Value where Key1 where Key2
      Key_normalized = Tuple{Traits.normalize_parametrictype.((Key1, Key2))...}
      subdict = get!($dict_traitsof2_typeparams, Key_normalized, Dict{Union{$TypeTuple2, Traits.Constrain2}, Type}())
      subdict[c2] = Union{Value, get(subdict, c2, Union{})}
    end

    function Base.setindex!(::typeof(traitsof), ::Type{Value}, ::Type{Key1}, ::Type{Key2}, ::Type{Key3}) where Value where Key1 where Key2 where Key3
      Key = Tuple{Key1, Key2, Key3}
      all_keys = (Key1, Key2, Key3)
      if all(Traits.hasnotypeparameters.(all_keys))
        $dict_traitsof3[Key] = Union{Value, get($dict_traitsof3, Key, Union{})}
      else
        Key_normalized = Tuple{Traits.normalize_parametrictype.(all_keys)...}
        subdict = get!($dict_traitsof3_typeparams, Key_normalized, Dict{Union{$TypeTuple3, Traits.Constrain3}, Type}())
        subdict[Key] = Union{Value, get(subdict, Key, Union{})}
      end
    end
    function Base.setindex!(::typeof(traitsof), ::Type{Value}, c3::Traits.Constrain3{Key1, Key2, Key3}) where Value where Key1 where Key2 where Key3
      Key_normalized = Tuple{Traits.normalize_parametrictype.((Key1, Key2, Key3))...}
      subdict = get!($dict_traitsof3_typeparams, Key_normalized, Dict{Union{$TypeTuple3, Traits.Constrain3}, Type}())
      subdict[c3] = Union{Value, get(subdict, c3, Union{})}
    end

    #This is not a macro because it always changes the traitsof definition
    # which was defined by the same ``@init_traitsof`` macro call
    # which also created this function.
    """
        refixate_traitsof()

    reset the generated code of traitsof
    hence subsequent calls of ``traitsof(...)``  will include all recent
    additions to ``traitsof`` (like ``traitsof[type] = traittype``)
    """
    refixate_traitsof() = eval(quote
      @generated function traitsof(::Type{T}) where T
        @assert isconcretetype(T) "calling traitsof() only supports concrete types"
        parametersofT = Traits.parametersof(T)
        # include traits from other_traitsof_functions
        u = Union{$((Expr(:call, fn, :T) for fn ∈ $other_traitsof_functions)...)}

        # union all traits of any parent
        for key ∈ Traits.supertypes(T)  # includes T itself
          if Traits.hasnotypeparameters(key)
            u = Union{u, get($$dict_traitsof1, key, Union{})} # we have the guarantee that this is a supertype
          else
            # TODO check whether subdict exists at all
            key_normalized = Traits.normalize_parametrictype(key)
            if haskey($$dict_traitsof1_typeparams, key_normalized)
              for (subkey, value) ∈ $$dict_traitsof1_typeparams[key_normalized]
                if subkey isa Traits.Constrain1
                  if T <: subkey.type && all(traitsrequire <: traitsof(p)
                      for (p, traitsrequire) ∈ zip(parametersofT, subkey.parameter_constraints)
                      if p isa Type)  # also values can be used as typeparameters as in Array{Int, 1}, however we don't have traits for values, but only for types
                    u = Union{u, value}
                  end
                else
                  if T <: subkey
                    u = Union{u, value}
                  end
                end
              end
            end
          end
        end
        :($u)
      end


      @generated function traitsof(::Type{T1}, ::Type{T2}) where T1 where T2
        types = (T1, T2)
        @assert all(isconcretetype.(types)) "calling traitsof() only supports concrete types"
        type = Tuple{T1, T2}
        type_parameters = Base.Iterators.flatten(Traits.parametersof.(types))
        # include traits from other_traitsof_functions
        u = Union{$((Expr(:call, fn, :T1, :T2) for fn ∈ $other_traitsof_functions)...)}

        # we go through all parent types
        for STs ∈ Iterators.product(Traits.supertypes.(types)...)
          # ST1, ST2 = STs
          key = Tuple{STs...}
          if all(Traits.hasnotypeparameters(st) for st ∈ STs) # this is short cycling
            u = Union{u, get($$dict_traitsof2, key, Union{})} # we have the guarantee that this is a supertype
          else
            key_normalized = Tuple{Traits.normalize_parametrictype.(STs)...}
            if haskey($$dict_traitsof2_typeparams, key_normalized)
              for (subkey, value) ∈ $$dict_traitsof2_typeparams[key_normalized]
                if subkey isa Traits.Constrain2
                  if type <: subkey.type && all(traitsrequire <: traitsof(p)
                      for (p, traitsrequire) ∈ zip(type_parameters, subkey.parameter_constraints)
                      if p isa Type)  # also values can be used as typeparameters as in Array{Int, 1}, however we don't have traits for values, but only for types
                    u = Union{u, value}
                  end
                else
                  if type <: subkey
                    u = Union{u, value}
                  end
                end
              end
            end
          end
        end
        :($u)
      end

      @generated function traitsof(::Type{T1}, ::Type{T2}, ::Type{T3}) where T1 where T2 where T3
        types = (T1, T2, T3)
        @assert all(isconcretetype.(types)) "calling traitsof() only supports concrete types"
        type = Tuple{T1, T2, T3}
        type_parameters = Base.Iterators.flatten(Traits.parametersof.(types))
        # include traits from other_traitsof_functions
        u = Union{$((Expr(:call, fn, :T1, :T2, :T3) for fn ∈ $other_traitsof_functions)...)}

        # union all traits of any parent
        for STs ∈ Iterators.product(Traits.supertypes.(types)...)
          # ST1, ST2, ST3 = STs
          key = Tuple{STs...}
          if all(Traits.hasnotypeparameters(st) for st ∈ STs)  # this is short cycling
            u = Union{u, get($$dict_traitsof3, key, Union{})}  # we have the guarantee that this is a supertype
          else
            key_normalized = Tuple{Traits.normalize_parametrictype.(STs)...}
            if haskey(dict_traitsof3_typeparams, key_normalized)
              for (subkey, value) ∈ $$dict_traitsof3_typeparams[key_normalized]
                if subkey isa Traits.Constrain3
                  if type <: subkey.type && all(traitsrequire <: traitsof(p)
                      for (p, traitsrequire) ∈ zip(type_parameters, subkey.parameter_constraints)
                      if p isa Type)  # also values can be used as typeparameters as in Array{Int, 1}, however we don't have traits for values, but only for types
                    u = Union{u, value}
                  end
                else
                  if type <: subkey
                    u = Union{u, value}
                  end
                end
              end
            end
          end
        end
        :($u)
      end
    end)
    # execute fixation initially
    # (is only really fixed if a traitsof of a Type is asked for
    #  and then also only for that type)
    refixate_traitsof()
  end)
end

# define Basic Traits for Base
@init_traitsof(BasicTraits.basictraits)

end # module
