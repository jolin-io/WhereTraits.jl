module Traits
export traitsof, TypeLowerBound, TypeLB
# Traits.functions needs to be accessed qualified, i.e. Traits.functions
include("Utils.jl")
using Base.Iterators
using .Utils

"""
    traitsof(type)

Return the trait types of a given Type ``type``. If the type has multiple traits, a Union Type is returned.

# Examples
```jldoctest
julia> traitsof(Integer)  # default value
Union{}
julia> traitsof(Integer) = Union{Integer, String}
julia> traitsof(Integer)
Union{Integer, String}
julia> struct MyType end
julia> traitsof(MyType)  # everything has Union{} as default value
Union{}
```
"""
# function traitsof end
# # default empty traits for everything to define default methods for trait-based dispatch
# traitsof(::Type{T}) where T = Union{}

# TODO support UnionAll Types!
# TODO implement this by searching for all methods matching the given arguments
# this would also fix the custom Union{} which currently have to be defined for overlapping traits when using Types with Typeparameters

# we use continuation style, see https://github.com/schlichtanders/Continuables.jl
build_traitsof(cont; max_arity = 2, max_typeparameters = 3) = for arity ∈ 1:max_arity
  varnames = Symbol.(:a, 1:arity)
  typenames = Symbol.(:T, 1:arity)
  vtnames = zip(varnames, typenames)
  signature = [:($v::Type{$t}) for (v, t) in vtnames]
  typeparameters_tuples = [:(parametersof($t)) for t in typenames]

  # base function to link traits on concrete DataTypes
  funcname0 = Symbol("traitsof", arity)
  # helper function to dispatch on number of typeparameters
  funcname = Symbol("_traitsof", arity)
  expr_urfunc = where(:(traitsof($(signature...))), typenames...)
  cont(:($expr_urfunc = Union{$funcname0($(varnames...)), $funcname($(varnames...), $(typeparameters_tuples...))}))

  # add default value
  expr_func0 = where(:($funcname0($(signature...))), typenames...)
  cont(:($expr_func0 = Union{}))

  paramtypenames_suffix = Symbol.(:P, 1:max_typeparameters)

  # go through all possible combinations of number of typeparameters
  for nparams ∈ selfproduct(0:max_typeparameters, arity)
    paramtypenames = [Symbol.(tpe, paramtypenames_suffix[1:nprm]) for (tpe, nprm) ∈ zip(typenames, nparams)]
    signature_tuples = [:(::Type{Tuple{$(ps...)}}) for ps in paramtypenames]
    paramtraitsof = [:(traitsof($p)) for ps in paramtypenames for p in ps]

    expr_func = where(:($funcname($(signature...), $(signature_tuples...))), typenames..., flatten(paramtypenames)...)
    lengths = length.(paramtypenames)

    if all(l -> l == 0, lengths)
      # if there are no typeparameters at all, we just return Union{}
      cont(:($expr_func = Union{}))
    else
      # else we can add extra dispatch methods for trait-based dispatch
      subfuncname = Symbol("traitsof", arity, Symbol.("_", lengths)...)
      cont(:($expr_func = $subfuncname($(varnames...), $(paramtraitsof...))))

      # default value Union{}
      paramvarnames = [Symbol.(var, paramtypenames_suffix[1:nprm]) for (var, nprm) ∈ zip(varnames, nparams)]
      signature_traits = [:($p::Type{$P}) for (p, P) in zip(flatten(paramvarnames), flatten(paramtypenames))]
      expr_subfunc = where(:($subfuncname($(signature...), $(signature_traits...))), typenames..., flatten(paramtypenames)...)
      cont(:($expr_subfunc = Union{}))
    end
  end
end

# we eval every expression
# Note, thanks to continuation style, that this is not allocating any Array with all Expr in it
build_traitsof(eval)

# to visualize what this is doing, do:
#=
l = Expr[]
build_traitsof(expr -> push!(l, expr))
Expr(:block, l...)
=#


# """
#     traitsof_push!(type, new_trait)
#
# Adds the ``new_trait`` Type to the given Type ``type``. I.e. the ``traitof`` is reassigned to the Union of the old combined with the new trait.
#
# Returns the new ``traitof(type)`` for convenience
#
# # Examples
# ```jldoctest
# julia> traitsof(Integer)
# Union{}
# julia> traitsof_push!(Integer, Integer)
# julia> traitsof(Integer)
# Integer
# julia> traitsof_push!(Integer, Union{Integer, String})
# julia> traitsof(Integer)
# Union{Integer, String}
# ```
# """
#
# function traitsof_push!(::Type{Which}, ::Type{With}) where Which where With
#   # reference to Main needs to be changed to whatever package name this gets
#   try
#     old_trait = traitsof1(Which)
#     # we need to overload traitsof1
#     @eval traitsof1(::Type{$Which}) = Union{$old_trait, $With}
#     # but can return from traitsof (which should refer to the same)
#     @eval traitsof($Which)
#   catch
#     # we need to overload traitsof1
#     @eval traitsof1(::Type{$Which}) = $With
#     # but can return from traitsof (which should refer to the same)
#     @eval traitsof($Which)
#   end
# end


# we overwrite index functions for ``traitsof`` function object
Base.getindex(::typeof(traitsof), args...) = traitsof(args...)

function Base.setindex!(::typeof(traitsof), ::Type{Value}, ::Type{Key}) where Value where Key
  old_trait = traitsof1(Key)
  # we need to overload traitsof1
  @eval traitsof1(::Type{$Key}) = Union{$old_trait, $Value}
  # written as index assignment, the return value is fixed to $Value by julia
  # it would make sense to return ``traitsof(Key)`` instead
  # traitsof(Key)
end

function Base.setindex!(::typeof(traitsof), ::Type{Value}, ::Type{Key1}, ::Type{Key2}) where Value where Key1 where Key2
  old_trait = traitsof2(Key1, Key2)
  # we need to overload traitsof2
  @eval traitsof2(::Type{$Key1}, ::Type{$Key2}) = Union{$old_trait, $Value}
  # written as index assignment, the return value is fixed to $Value by julia
  # it would make sense to return ``traitsof(Key1, Key2)`` instead
  # traitsof(Key1, Key2)
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
end # module
