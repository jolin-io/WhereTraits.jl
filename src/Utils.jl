module Utils
export parametersof, selfproduct, chain,
  where, invoke_method, normalize_parametrictype, supertypes, hasnotypeparameters
using Base.Iterators

# Types
# =====

parametersof(::Type{T}) where T = Base.unwrap_unionall(T).parameters



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

end  # module
