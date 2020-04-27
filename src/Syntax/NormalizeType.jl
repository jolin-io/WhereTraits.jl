module NormalizeType
export normalize_typevars, split_typevar

using Traits.Utils
using ExprParsers

const VectorLike = Union{Vector, Tuple, Core.SimpleVector}
const True = Val{true}
const False = Val{false}
const TrueFalse = Union{True, False}
Base.:(!)(::True) = False()
Base.:(!)(::False) = True()

"""
when normalizing a type signature, we have need to distinguish TypeVar according to three different context
- whether we need to track the concrete normalization-renaming (normalize_track)
- whether we just need to normalize (normalize_notrack)
- or whether a typevar is not to be normalized (neither of the other)
"""
Base.@kwdef struct TypeVarContext
  normalize_notrack::Vector{TypeVar} = TypeVar[]
  normalize_track::Vector{TypeVar} = TypeVar[]
end

"""
traverses given types, renaming all typeparameters to normalized name

returns 3-tuple
- type with substituted typevars
- list of function level typevars (includable into where-statement)
- dict mapping new typevar to old once
"""
function normalize_typevars(types::VectorLike, typevars_track_normalization)
  tuple_parents_only = True()
  countfrom_typevars! = Base.Iterators.Stateful(Base.Iterators.countfrom(1))
  typevar_old_to_new! = Dict{TypeVar, Symbol}()
  typevar_context = TypeVarContext(normalize_track = typevars_track_normalization)
  typeexprs, typevars = _normalize_typevars(types, typevar_context, typevar_old_to_new!, countfrom_typevars!, tuple_parents_only)
  typevar_new_to_old = Dict(v => k.name for (k, v) in typevar_old_to_new!)
  typeexprs, typevars, typevar_new_to_old
end

function _normalize_typevars(types::VectorLike, typevar_context, typevar_old_to_new!, countfrom!, tuple_parents_only::TrueFalse)
  typeexprs = []
  typevars = []
  for p in types
    _typeexpr, _typevars = _normalize_typevars(p, typevar_context, typevar_old_to_new!, countfrom!, tuple_parents_only)
    push!(typeexprs, _typeexpr)
    append!(typevars, _typevars)
  end
  typeexprs, typevars
end

function _simplify_type(varargexpr::Expr)
  parsed = parse_expr(EP.Type(), varargexpr)

  isdroppable(other) = false
  isdroppable(sym::Symbol) = sym in parsed.wheres

  droppable_curlies = []
  for sym in reverse(parsed.curlies)
    isdroppable(sym) || break
    push!(droppable_curlies, sym)
  end
  parsed.curlies = filter(x -> x ∉ droppable_curlies, parsed.curlies)
  parsed.wheres = filter(x -> x ∉ droppable_curlies, parsed.wheres)
  to_expr(parsed)
end


# UnionAlls and abstracttypes are treated differently in that they will create function level typevariables
function _normalize_typevars(type::Type, typevar_context, typevar_old_to_new!, countfrom!, tuple_parents_only::True)
  typebase, extra_typevars = split_typevar(type)
  name = _name(typebase)

  if name === :Vararg
    # Vararg behave different than all other types when used as TypeVar for Tuple, namely
    # Tuple{Vararg{T} where T} != (Tuple{Vararg{T}} where T)
    # which is the same as if tuple_parents_only == False
    varargexpr, typevars = _normalize_typevars(type, typevar_context, typevar_old_to_new!, countfrom!, False())

    # we need to simplify the type because of a bug in julia
    # https://discourse.julialang.org/t/inconsistent-behaviour-of-vararg-unionall/31644
    _simplify_type(varargexpr), typevars

  elseif isabstracttype(type) || type isa UnionAll || name === :Union
    # each abstract or unionall type or Union type can have different concrete types
    # hence it makes sense to give it a typevariable like
    # Tuple{Vector} <: Tuple{V} where {V <: Vector}
    # more concretely, the typeparameter representing this type must
    # come after the typeparameters of the type
    # Tuple{Vector{T} where T} <: Tuple{V} where {T, V <: Vector{T}}

    tuple_parents_only = name === :Tuple ? True() : False()
    # extra_typevars need to get normalized too
    typevar_context′ = TypeVarContext(
      normalize_track = typevar_context.normalize_track,
      normalize_notrack = [typevar_context.normalize_notrack; extra_typevars],
    )
    typeexpr, typevars = _normalize_typevars(typebase, typevar_context′, typevar_old_to_new!, countfrom!, tuple_parents_only)

    # add type variable representing the entire type
    i = next!(countfrom!)
    new = normalized_typevar_by_position(i, typeexpr)
    # we replace the given type with the new typevariablename
    # and add the constraint to the function level typevars
    new.name, [typevars; new]

  elseif isempty(type.parameters)  # concrete DataType without typeparameters
    type, []

  else
    # recurse into typeparameters of concrete DataType
    tuple_parents_only = name === :Tuple ? True() : False()
    typeexprs, typevars = _normalize_typevars(typebase.parameters, typevar_context, typevar_old_to_new!, countfrom!, tuple_parents_only)
    Expr(:curly, name, typeexprs...), typevars
  end
end
_name(type::Type) = type.name.name
_name(type::Union) = :Union


function _normalize_typevars(tv::TypeVar, typevar_context, typevar_old_to_new!, countfrom!::Base.Iterators.Stateful, tuple_parents_only::TrueFalse)
  if tv in keys(typevar_old_to_new!)
    # we may have complex interactions like diagonal types and other reuses of typevariables
    # hence we make sure we replace a variable only once
    new_name = typevar_old_to_new![tv]
    # we replace the typevar with new_name, but there is no new typevar
    new_name, []
  elseif tv in typevar_context.normalize_track
    i = next!(countfrom!)
    lowerbound, lb_vars = _normalize_typevars(tv.lb, typevar_context, typevar_old_to_new!, countfrom!, False())
    upperbound, ub_vars = _normalize_typevars(tv.ub, typevar_context, typevar_old_to_new!, countfrom!, False())
    new = normalized_typevar_by_position(i, lowerbound, upperbound)
    typevar_old_to_new![tv] = new.name
    new.name, [lb_vars; ub_vars; new]
  elseif tv in typevar_context.normalize_notrack
    # notrack, means this is implicit
    # hence all these typevars are independent and can just be replaced
    i = next!(countfrom!)
    lowerbound, lb_vars = _normalize_typevars(tv.lb, typevar_context, typevar_old_to_new!, countfrom!, False())
    upperbound, ub_vars = _normalize_typevars(tv.ub, typevar_context, typevar_old_to_new!, countfrom!, False())
    new = normalized_typevar_by_position(i, lowerbound, upperbound)
    new.name, [lb_vars; ub_vars; new]
  else  # this is a nested UnionAll somewhere
    tv.name, []
  end
end

# we need to solve ambiguities with type::Union (vs type::Type) and tuple_parents_only::TrueFalse (vs ::True)
function _normalize_typevars(type::Union, typevar_context, typevar_old_to_new!, countfrom!, tuple_parents_only::True)
  _normalize_typevars_unions(type, typevar_context, typevar_old_to_new!, countfrom!, tuple_parents_only)
end
function _normalize_typevars(type::Union, typevar_context, typevar_old_to_new!, countfrom!, tuple_parents_only::False)
  _normalize_typevars_unions(type, typevar_context, typevar_old_to_new!, countfrom!, tuple_parents_only)
end
function _normalize_typevars_unions(type::Union, typevar_context, typevar_old_to_new!, countfrom!, tuple_parents_only::TrueFalse)
  typeexprs, typevars = _normalize_typevars(_collect_union_types(type), typevar_context, typevar_old_to_new!, countfrom!, False())
  Expr(:curly, :Union, typeexprs...), typevars
end
_collect_union_types(any) = [any]
_collect_union_types(type::Union) = [type.a; _collect_union_types(type.b)]


function _normalize_typevars(type::DataType, typevar_context, typevar_old_to_new!, countfrom!, ::False)
  # if tuple_parents_only == False, ``type`` cannot be a Vararg, as Varargs are only allowed in Tuples
  if isempty(type.parameters)  # concrete DataType without typeparameters
    type, []
  else
    # recurse into typeparameters of concrete DataType
    typeexprs, typevars = _normalize_typevars(type.parameters, typevar_context, typevar_old_to_new!, countfrom!, False())
    Expr(:curly, type.name.name, typeexprs...), typevars
  end
end

function _normalize_typevars(type::UnionAll, typevar_context, typevar_old_to_new!, countfrom!, ::False)
  # if there is not everywhere tuple types above, this UnionAll stays as it is, only renaming the inner Type
  typebase, typevars_unionall = split_typevar(type)
  typevars_unionall_names = [tv.name for tv in typevars_unionall]
  typevars_track_unshadowed = [tv for tv ∈ typevar_context.normalize_track if tv.name ∉ typevars_unionall_names]
  typevars_notrack_unshadowed = [tv for tv ∈ typevar_context.normalize_notrack if tv.name ∉ typevars_unionall_names]
  typevar_context′ = TypeVarContext(
    normalize_track = typevars_track_unshadowed,
    normalize_notrack = typevars_notrack_unshadowed,
  )
  typebase_expr_new, typevars_new = _normalize_typevars(typebase, typevar_context′, typevar_old_to_new!, countfrom!, False())
  typevars_unionall_new, typevars_new′ = _normalize_unionall(typevars_unionall, typevar_context′, typevar_old_to_new!, countfrom!, False())
  _expr_rewrap_typevars(typebase_expr_new, typevars_unionall_new), [typevars_new; typevars_new′]
end

function _normalize_unionall(types::VectorLike, typevar_context, typevar_old_to_new!, countfrom!::Base.Iterators.Stateful, tuple_parents_only::TrueFalse)
  typeexprs = []
  typevars = []
  for p in types
    _typeexpr, _typevars = _normalize_unionall(p, typevar_context, typevar_old_to_new!, countfrom!, False())
    push!(typeexprs, _typeexpr)
    append!(typevars, _typevars)
  end
  typeexprs, typevars
end
function _normalize_unionall(tv::TypeVar, typevar_context, typevar_old_to_new!, countfrom!::Base.Iterators.Stateful, tuple_parents_only::TrueFalse)
  lowerbound, lb_vars = _normalize_typevars(tv.lb, typevar_context, typevar_old_to_new!, countfrom!, False())
  upperbound, ub_vars = _normalize_typevars(tv.ub, typevar_context, typevar_old_to_new!, countfrom!, False())
  to_expr(EP.TypeRange_Parsed(lowerbound, tv.name, upperbound)), [lb_vars; ub_vars]
end

# anything else, like Int or Symbol TypeVars
_normalize_typevars(any, typevars_old, typevar_old_to_new!, countfrom!, ::TrueFalse) = any, []
_normalize_typevars(bounds::Type{Union{}}, _, _, _, ::True) = bounds, []
_normalize_typevars(bounds::Type{Union{}}, _, _, _, ::False) = bounds, []

normalized_typevar_by_position(position::Int) = EP.TypeRange_Parsed(Union{}, Symbol("T", position), Any)
normalized_typevar_by_position(position::Int, ub) = EP.TypeRange_Parsed(Union{}, Symbol("T", position), ub)
normalized_typevar_by_position(position::Int, lb, ub) = EP.TypeRange_Parsed(lb, Symbol("T", position), ub)

split_typevar(base) = base, TypeVar[]
function split_typevar(t::UnionAll)
  base, typevars = split_typevar(t.body)
  base, [t.var; typevars...]
end

function _expr_rewrap_typevars(typeexpr::Expr, typevars)
  Expr(:where, typeexpr, typevars...)
end
function _expr_rewrap_typevars(typeexpr::Expr, typevars::Vector{TypeVar})
  _expr_rewrap_typevars(typeexpr, to_expr.(typevars))
end

end  # module
