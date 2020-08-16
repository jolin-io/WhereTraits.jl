# Internal State of the syntax
# ============================

module InternalState
export get_traitsstore, get_traitsstores
using WhereTraits.Utils
using StructEquality
using ExprParsers
using ProxyInterfaces


@def_structequal Base.@kwdef struct DefOuterFuncFixedPart{Signature}
    # everything under fixed should be identifiable via signature
    signature::Type{Signature}
    name::Union{Symbol, Expr}
    curlies::Vector{Union{Symbol, Expr}}
    args::Vector{EP.Arg_Parsed}
    wheres::Vector{EP.TypeRange_Parsed}
    # body information, also fixed by signature, normalized naming
    innerargs_args::Vector{Symbol}
    innerargs_typevars::Vector{Symbol}
end
@def_structequal Base.@kwdef struct DefOuterFuncNonFixedPart
  innerargs_traits::Vector{Union{Symbol, Expr}}
end
@def_structequal Base.@kwdef struct DefOuterFunc{Signature}
  fixed::DefOuterFuncFixedPart{Signature}
  nonfixed::DefOuterFuncNonFixedPart
end

@def_structequal Base.@kwdef struct DefInnerFuncFixedPart
  args_mapping::Dict{Symbol, Symbol}
  typevars_mapping::Dict{Symbol, Symbol}
  traits_mapping::Dict{Union{Symbol, Expr}, Union{Symbol, Expr}}
end
@def_structequal Base.@kwdef struct DefInnerFuncNonFixedPart
  kwargs::Vector{Expr}
  body::Expr
  expr_original::Expr
end
@def_structequal Base.@kwdef struct DefInnerFunc
  fixed::DefInnerFuncFixedPart
  nonfixed::DefInnerFuncNonFixedPart
end

const InnerFuncs = Dict{DefInnerFuncFixedPart, DefInnerFuncNonFixedPart}
@def_structequal Base.@kwdef struct DefTraitsFunction{Signature}
  outer::DefOuterFunc{Signature}
  inners::InnerFuncs
end


@def_structequal Base.@kwdef struct Reference
  mod::Module
  name::Symbol
end
ExprParsers.to_expr(r::Reference) = :($(r.mod).$(r.name))

@def_structequal Base.@kwdef struct TraitsStore{Signature}
  original_function::Reference
  # maps a type signature to the respective outerfunction with possible several innerfunction-definitions
  definitions::DefTraitsFunction{Signature}
end

"""
returns TraitsStore or nothing if no TraitsStore could be found
"""
function get_traitsstore(mod, funcname, signature)
  mod_original, funcname_original = normalize_mod_and_name(mod, funcname)
  try
    # call with special `traitsstore` argument to get the store
    return getproperty(mod_original, funcname_original)(traitsstoresingleton, signature)
  catch e
    e isa Union{UndefVarError, MethodError} || rethrow()
    return nothing
  end
end


abstract type TraitsSingleton end

"""
    traitsdefsingleton

Used to mark a function method as belonging to the traitsdefinition
"""
struct TraitsDefSingleton <: TraitsSingleton end
const traitsdefsingleton = TraitsDefSingleton()

"""
    traitsstoresingleton

Used to mark a function method as belonging to the traitsstore
"""
struct TraitsStoreSingleton <: TraitsSingleton end
const traitsstoresingleton = TraitsStoreSingleton()

"""
    traitsdocsingleton

Used to mark a function method as used for the auto documentation feature
"""
struct TraitsDocSingleton <: TraitsSingleton end
const traitsdocsingleton = TraitsDocSingleton()

end # module
