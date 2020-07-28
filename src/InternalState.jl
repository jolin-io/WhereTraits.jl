# Internal State of the syntax
# ============================

module InternalState
export getorcreate_traitsstore
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
@def_structequal struct DefTraitsFunction{Signature}
  outer::DefOuterFunc{Signature}
  inners::InnerFuncs
end


@def_structequal Base.@kwdef struct Reference
  mod::Module
  name::Symbol
end
ExprParsers.to_expr(r::Reference) = :($(r.mod).$(r.name))

@def_structequal Base.@kwdef struct TraitsStore
  original_function::Reference
  # maps a type signature to the respective outerfunction with possible several innerfunction-definitions
  definitions::TypeDict{DefTraitsFunction}
end
TraitsStore(original_function::Reference) = TraitsStore(original_function, TypeDict{DefTraitsFunction}())

ProxyInterfaces.dict(store::TraitsStore) = store.definitions
ProxyInterfaces.dict(Store::Type{TraitsStore}) = TypeDict{DefTraitsFunction}
ProxyInterfaces.@dict_mutable TraitsStore
Base.copy(store::TraitsStore) = TraitsStore(store.original_function, copy(store.definitions))


"""
returns (original module, TraitsStore)
"""
function getorcreate_traitsstore(mod, funcname)
  mod_original, funcname_original = normalize_mod_and_name(mod, funcname)
  traitsstore = try
    # call with special `traitsstate` argument to get the store
    getproperty(mod_original, funcname_original)(traitssingleton)
  catch e
    e isa Union{UndefVarError, MethodError} || rethrow()
    # if nothing is defined yet return empty TraitsStore
    TraitsStore(Reference(mod_original, funcname_original))
  end
  traitsstore
end

struct TraitsSingleton end
const traitssingleton = TraitsSingleton()

end # module
