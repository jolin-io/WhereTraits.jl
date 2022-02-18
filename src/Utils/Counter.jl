Base.@kwdef struct Counter{T}
    start::T = 1
    step::T = 1
end
Base.iterate(counter::Counter) = (counter.start, counter.start)
function Base.iterate(counter::Counter, state)
    next = state + counter.step
    (next, next)
end
Base.IteratorSize(::Type{<:Counter}) = Base.SizeUnknown()
Base.IteratorEltype(::Type{<:Counter}) = Base.HasEltype()
Base.eltype(::Type{Counter{T}}) where T = T