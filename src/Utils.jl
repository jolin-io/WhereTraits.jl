module Utils
export parametersof, selfproduct, where
using Base.Iterators

# Types
# =====

# this seems valid application of @pure
# see https://discourse.julialang.org/t/is-this-a-valid-pure-function/17577
"""
    parametersof(Dict{String, Int}) = Tuple{String, Int}

extract type parameters like ``f(t::Type) = t.parameters`` however with inference support
never use `.parameters` again
"""
Base.@pure parametersof(t::Type) = Tuple{t.parameters...}

# alternativ implementation using generated functions
# @generated function getParameters(t::Type)
#   :($(Tuple{t.parameters[1].parameters...}))
# end


function where(e, ts...)
  for t ∈ ts
    e = Expr(:where, e, t)
  end
  e
end

# Iterators
# =========

# we need to ``collect`` here, as it will transform
# the nested tuple output of Iterators.product
# into a linearly iterable n-dim array
selfproduct(itr, n::Int) = collect(product((itr for i in 1:n)...))

end  # module
