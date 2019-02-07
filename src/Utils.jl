module Utils
export parametersof, selfproduct, chain,
  where, invoke_method, normalize_parametrictype, supertypes,
  hasnotypeparameters, DefaultDict, extract_name, merge_module_attr
using Base.Iterators

# Types
# =====

parametersof(::Type{T}) where T = Base.unwrap_unionall(T).parameters


toplevel_module() = @__MODULE__
macro toplevel_module2()
  __module__
end

function normalize_parametrictype(::Type{T}) where T
  PlainT = Base.unwrap_unionall(T)
  getfield(PlainT.name.module, PlainT.name.name)
end

function supertypes(::Type{T}) where T
  tps = []
  i = T
  while i !== Any
    push!(tps, i)
    i = supertype(i)
  end
  tuple(tps...)
end

function hasnotypeparameters(::Type{T}) where T
  length(Base.unwrap_unionall(T).parameters) == 0
end

# Meta
# ====

extract_name(expr::Symbol) = expr
extract_name(expr::QuoteNode) = expr.value
function extract_name(expr::Expr)
  if expr.head == :.
    extract_name(expr.args[2])
  else
    error("Expr not supported: $expr")
  end
end

# apparently Expr(:., ..) are combined with QuoteNode instead of Symbol
# there is also special syntax for those cases
merge_module_attr(mod, attr::QuoteNode) = Expr(:., mod, attr)
merge_module_attr(mod, attr::Symbol) = merge_module_attr(mod, QuoteNode(attr))
function merge_module_attr(mod, attr::Expr)
  @assert attr.head == :.
  merge_module_attr(merge_module_attr(mod, attr.args[1]), attr.args[2])
end

function where(e, ts...)
  for t ∈ ts
    e = Expr(:where, e, t)
  end
  e
end


function invoke_method(m::Method, args...; kwargs...)
  invoke(m.sig.parameters[1].instance, parametersofmethod(m), args...; kwargs...)
end


# Iterators
# =========

# we need to ``collect`` here, as it will transform
# the nested tuple output of Iterators.product
# into a linearly iterable n-dim array
selfproduct(itr, n::Int) = collect(product((itr for i in 1:n)...))

chain(itr...) = flatten(itr)


# simple DefaultDict
# ==================

# delegate copied from DataStructures.jl
# --------------------------------------

function unquote(e::Expr)
    @assert e.head == :quote
    e.args[1]
end
unquote(e::QuoteNode) = e.value


macro delegate(source, targets)
  typename = esc(source.args[1])
  fieldname = unquote(source.args[2])
  funcnames = targets.args
  n = length(funcnames)
  fdefs = Vector{Any}(undef, n)
  for i in 1:n
    funcname = esc(funcnames[i])
    fdefs[i] = quote
                 ($funcname)(a::($typename), args...; kwargs...) =
                   ($funcname)(a.$fieldname, args...; kwargs...)
               end
  end
  Expr(:block, fdefs...)
end

# DefaultDict
# -----------

struct DefaultDict{K,V}
  # Function: key -> new default value
  # first place to support do syntax
  default::Function
  mutate_on_getindex::Bool
  dict::Dict{K,V}
end
# we default to mutate_on_getindex = false because external tools will access d[key] and assume this won't mutate anything
DefaultDict{K,V}(default::Function, mutate_on_getindex::Bool = false) where K where V = DefaultDict{K,V}(default, mutate_on_getindex, Dict{K,V}())

# delegate only a few methods so far
@delegate DefaultDict.dict [Base.setindex!, Base.haskey, Base.getkey, Base.iterate]

# custom getters

function Base.get(d::DefaultDict, key)
  Base.get(d.dict, key, d.default(key))
end

function Base.get!(d::DefaultDict, key)
  Base.get!(d.dict, key, d.default(key))
end

function Base.getindex(d::DefaultDict, key...)
  if length(key) == 1
    key = key[1]
  end

  if d.mutate_on_getindex
    Base.get!(d, key)
  else
    Base.get(d, key)
  end
end

end # module
