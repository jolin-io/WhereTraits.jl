module Traits
export @traitsof_init, TypeLowerBound, TypeLB, Constrain, Constrain1, Constrain2, Constrain3, Traitsof, @traitsof_link

# TODO think about supporting traits for Union types, like Union{Int, Nothing} or Union{Double, Missing}

# Traits.functions needs to be accessed qualified, i.e. Traits.functions
include("Utils.jl")
include("BasicTraits.jl")
using .Utils
using Base.Iterators

"""
all subtypes should implement the following fields:
```julia
type::Type{Tuple{T1, T2, T3, ..., TN}}
parameter_constraints::Tuple
```
where `length(parameter_constraints) == length(type.parameters)`, i.e. one constraint per Type T

Each constraint, is just a Union of traits which you the parameter to fulfil.
If there is no constraint required use `Union{}`
"""
abstract type Constrain{N} end
"""
support complex dependencies on typeparameters for a single type
"""
struct Constrain1{T} <: Constrain{1}
  type::Type{Tuple{T}}
  # we would need typeof() instead of Type{} because tuple(Dict) isa Tuple{typeof(Dict)} but not a Tuple{Type{Dict}}
  # however typeof(T) gives TypeVar as this apparently is evaluated at an earlier stage
  # hence no types support
  # types::Tuple{typeof(T)}
  parameter_constraints::Tuple

  function Constrain1{T}(parameter_constraints...) where T
    if length(parametersof(T)) != length(parameter_constraints)
      error("need exactly as many parameter trait specifications as type ``$T`` has parameters")
    end
    new{T}(Tuple{T}, parameter_constraints)
  end
end

"""
support complex dependencies on typeparameters for two types
"""
struct Constrain2{T1, T2} <: Constrain{2}
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
struct Constrain3{T1, T2, T3} <: Constrain{3}
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

short ``TypeLB``.

# Examples
```jldoctest
julia> TypeLB(Union{Integer, String})
Type{T} where T>:Union{Integer, String}
```
for convenience also the following works
```jldoctest
julia> TypeLB(Integer, String)
Type{T} where T>:Union{Integer, String}
```
"""
TypeLowerBound(::Type{LB}) where LB = Type{>:LB}
TypeLowerBound(::Type{A}, ::Type{B}) where A where B = Type{>:Union{A, B}}
TypeLowerBound(::Type{A}, ::Type{B}, ::Type{C}) where A where B where C = Type{>:Union{A, B, C}}
TypeLowerBound(::Type{A}, ::Type{B}, ::Type{C}, ::Type{D}) where A where B where C where D = Type{>:Union{A, B, C, D}}
TypeLowerBound(::Type{A}, ::Type{B}, ::Type{C}, ::Type{D}, ::Type{E}) where A where B where C where D where E = Type{>:Union{A, B, C, D, E}}
TypeLB = TypeLowerBound



"""
abstract super type for traitsof functions to enable easier dispatching

Think Traitsof as a struct type with the following properties:
  ...TODO copy struct type signature

It is not a struct itself, because we want to be able to overwrite the function call
  syntax of each single traitsof instance, which requires subtyping.
"""
abstract type Traitsof end

Base.iterate(x::Traitsof) = (x, nothing)
Base.iterate(x::Traitsof, ::Any) = nothing

Base.getindex(traitsof::Traitsof, args...) = traitsof(args...)

function Base.setindex!(traitsof::Traitsof, ::Type{Value}, types::Vararg{Type, N}) where Value where N
  if all(Traits.hasnotypeparameters.(types))
    traitsof.notypeparams[N][types] = Union{Value, traitsof.notypeparams[N][types]}
  else
    types_normalized = Traits.normalize_parametrictype.(types)
    # create empty subdict if not existing
    subdict = get!(traitsof.typeparams[N], types_normalized)
    v = subdict[types]
    Base.setindex!(subdict, Union{Value, v}, types)
    subdict[types] = Union{Value, subdict[types]}
  end
end

function Base.setindex!(traitsof::Traitsof, ::Type{Value}, c::Constrain{N}) where Value where N
  types_normalized = Traits.normalize_parametrictype.(tuple(c.type.parameters...))
  # create empty subdict if not existing
  subdict = get!(traitsof.typeparams[N], types_normalized)
  subdict[c] = Union{Value, subdict[c]}
end


"""
generic function to call traitsof

this is used internally to construct the generated functions for traitsof
"""
function call_traitsof(traitsof::Traitsof, types::NTuple{N, Type}) where N
  @assert all(isconcretetype.(types)) "calling traitsof() only supports concrete types"

  type = Tuple{types...}
  types_parameters = Base.Iterators.flatten(Traits.parametersof.(types))

  # include traits from parent_functions
  u::Type = Union{(fn(types...) for fn ∈ traitsof.parent_functions)...}

  # we go through all possible combinations of supertypes
  for supertypes ∈ Iterators.product(Traits.supertypes.(types)...)  # Traits.supertypes includes the type itself
    if all(Traits.hasnotypeparameters(st) for st ∈ supertypes) # this is short-cycling
      u = Union{u, traitsof.notypeparams[N][supertypes]} # we have the guarantee that this is a supertype
    else
      supertypes_normalized = Traits.normalize_parametrictype.(supertypes)
      # for traitsof(type with typeparams) also include all ancestors_traitsof and their parents_traitsof and so on
      all_typeparams = flatten(trof.typeparams[N][supertypes_normalized] for trof in chain(traitsof, traitsof.ancestors_traitsof))
      for (key, value) ∈ all_typeparams
        if key isa Traits.Constrain{N}
          if type <: key.type && all(traitsrequire <: traitsof(param)
              for (param, traitsrequire) ∈ zip(types_parameters, key.parameter_constraints)
              if param isa Type)  # also values can be used as typeparameters as in Array{Int, 1}, however we don't have traits for values, but only for types
            u = Union{u, value}
          end
        else
          # if key is not a Constrain, then key can still be a Tuple of specific UnionAll types which may match or not
          # we first need to construct a TupleType from the Tuple key to easily check the inheritage
          if type <: Tuple{key...}
            u = Union{u, value}
          end
        end
      end
    end
  end
  u
end


"""
binds a functions that expects Traitsof as first argument to the current traitsof

general recommendation how to link specific functions to the module's own traitsof function:

use as:
```
@traitsof_link OtherModule.function_expecting_traitsof_as_first_argument

@traitsof_link begin
  OtherModule.funcA
  OtherModule.funcB
end

@traitsof_link OtherModule begin
  funcA
  funcB
end
```
"""
macro traitsof_link(func)
  if func.head == :block
    funcs = filter(func.args) do expr
      expr isa Union{Expr, Symbol, QuoteNode}
    end
    Expr(:block, single_traitsof_link.(funcs)...)
  else
    single_traitsof_link(func)
  end
end
macro traitsof_link(mod, funcs)
  @assert funcs.head == :block
  funcs = filter(funcs.args) do expr
    expr isa Union{Expr, Symbol, QuoteNode}
  end
  funcs = [merge_module_attr(mod, f) for f in funcs]
  Expr(:block, single_traitsof_link.(funcs)...)
end


function single_traitsof_link(func)
  funcname = extract_name(func)
  esc(quote
    $funcname(args...; kwargs...) = $func(traitsof, args...; kwargs...)
  end)
end




"""
eval the definition of traitsof in the current module

This is mainly used to easily precompile traitsof.
Preventing type piracy is also targeted, but not so much, as traits can only be added to.
Still, by adding certain traits you can break others code.

With your own traitsof definition, such problems are all fixed.
"""
macro traitsof_init(parent_functions...)

  # we use a hidden symbol for the ConcreteType, as this should be a singleton type
  # and is only constructed to easily recompile generated functions per package
  ConcreteTraitsof = :ConcreteTraitsof

  esc(quote
    # escape this, as we want to define
    # the traitsof function in the calling environment

    # TODO adapt string representation to present module which it is defined in
    # TODO adapt doc
    """
        traitsof(type)  # query current generated traits for ``type``
        traitsof[type]  # == traitsof(type)
        traitsof[type] = TraitType  # add new ``TraitType`` as a trait to ``type``

        traitsof()  # regenerate Traits from internal dictionari_refixatees

    Return the trait types of a given Type ``type``. If the type has multiple traits, a Union Type is returned.

    # Examples
    ```jldoctest
    julia> traitsof(Integer)  # default value
    Union{}
    julia> traitsof[Integer] = Union{Integer, String}
    julia> traitsof[Integer]
    julia> Union{}  # traitsof is generated function and hence need to be recompiled to change values
    julia> traitsof()  # with th_refixateis
    julia> traitsof[Integer]
    Union{Integer, String}
    julia> traitsof(Integer)  # [] or () - both do the same
    Union{Integer, String}
    julia> struct MyType end
    julia> traitsof(MyType)  # everything has Union{} as default value
    Union{}
    ```
    """
    struct $ConcreteTraitsof <: Traits.Traitsof
      parent_functions::Vector{Any}
      ancestors_traitsof::Vector{Traits.Traitsof}  # recursive


      # we partition the lookup for traits by 1) arity, and 2) typeparameters
      # where we create lookup tables for all respective types and unionall types

      # for types without typeparameters we can directly map the type to a respective collection of Traits
      notypeparams::Traits.DefaultDict{Int, Traits.DefaultDict{NTuple{N, Type}, Type} where N}
      # types with typeparameters are looked up in two stages
      # first as the generalized UnionAll to summarize all different types around that parametric type
      # second as a concrete mapping of types to traits
      typeparams::Traits.DefaultDict{Int, Traits.DefaultDict{NTuple{N, Type}, Traits.DefaultDict{Union{NTuple{N, Type}, Traits.Constrain{N}}, Type}} where N}

      function $ConcreteTraitsof(parent_functions)  # TODO create global parameter for default N?
        parent_traitsof = [traitsof for traitsof ∈ parent_functions if traitsof isa Traitsof]
        ancestors_traitsof = [parent_traitsof; (p.ancestors_traitsof for p ∈ parent_traitsof)...]

        # initialize DefaultDicts
        # always store top dict (there shouldn't be much requests/memory overhead and it simplifies the AP)
        notypeparams = Traits.DefaultDict{Int, Traits.DefaultDict{NTuple{N, Type}, Type} where N}(true) do N
          # don't store final default type automatically
          Traits.DefaultDict{NTuple{N, Type}, Type}(false) do _
            Union{}
          end
        end
        # always store top dict
        typeparams = Traits.DefaultDict{Int, Traits.DefaultDict{NTuple{N, Type}, Traits.DefaultDict{Union{NTuple{N, Type}, Traits.Constrain{N}}, Type}} where N}(true) do N
          # don't always store subdicts, as this will be asked
          Traits.DefaultDict{NTuple{N, Type}, Traits.DefaultDict{Union{NTuple{N, Type}, Traits.Constrain{N}}, Type}}(false) do _
            # don't store final default type automatically
            Traits.DefaultDict{Union{NTuple{N, Type}, Traits.Constrain{N}}, Type}(false) do _
              Union{}
            end
          end
        end
        new(collect(parent_functions), ancestors_traitsof, notypeparams, typeparams)
      end
    end

    # this should be the only instance of $ConcreteTraitsof
    # all dispatching is done via the abstract type Traits.Traitsof
    const traitsof = $ConcreteTraitsof(tuple($(parent_functions...)))


    # This is not a macro because it has to change always the same traitsof definition
    # which was defined by the same ``@init_traitsof`` macro call
    """
        traitsof_refixate()

    reset the generated code of traitsof
    hence subsequent calls of ``traitsof(...)``  will include all recent
    additions to ``traitsof`` (like ``traitsof[type] = MyNewTraitType``)
    """
    function traitsof_refixate()
      maxN = 3  # TODO change to global config parameter
      # we need to go through different arities manually to be able to use generated functions while extracting type information
      for N in 1:maxN
        typenames = Symbol.(:T, 1:N)
        args = [:(_::Type{$tn}) for tn in typenames]
        # we have to ignore the given traitsof object in the functionhead because this is a generated function,
        # i.e. we only get the Type and not the object itself
        # luckily this traitsof is uniquely defined and already directly referencable
        functionhead = Traits.where(:((::typeof(traitsof))($(args...))), typenames...)  # $$ConcreteTraitsof
        eval(quote
          # got parse error when using function $functionhead instead
          # luckily the second function syntax works
          @generated $functionhead = begin
            traits = Traits.call_traitsof($traitsof, tuple($(typenames...)))
            :($traits)
          end
        end)
      end
    end
    # execute fixation initially
    # (is only really fixed if a traitsof of a Type is asked for
    #  and then also only for that type)
    traitsof_refixate()
  end)
end

# define Basic Traits for Base
@traitsof_init(BasicTraits.basictraits)

end # module
