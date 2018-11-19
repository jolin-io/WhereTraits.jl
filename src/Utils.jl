module Utils
export parametersof, parametersofmethod, firstparameterof, selfproduct, chain, where, invoke_method
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
# @generated function parametersof(t::Type)
#   :($(Tuple{t.parameters[1].parameters...}))
# end


"""
similar helper like `parametersof`

especially for working with method signatures where first parameter element needs to be ignored
"""
Base.@pure parametersofmethod(m::Method) = Base.rewrap_unionall(Tuple{Base.unwrap_unionall(m.sig).parameters[2:end]...}, m.sig)
# TODO make this pure again if possible. Like reimplementing rewrap_unionall and unwrap_unionall


Base.@pure firstparameterof(t::Type) = t.parameters[1]



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
