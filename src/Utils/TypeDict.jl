
struct TypeDict{T}
  # we use Tuple instead of Pair because it is covariant in respect to subtyping
  _list::Vector{Tuple{Type, T}}

  function TypeDict{T}() where T
    new{T}(Vector{Tuple{Type, T}}())
  end
  function TypeDict{T}(args::Vector{Tuple{Type, T}}) where T
    # 2x reverse because later should overwrite earlier
    vector = reverse(unique(x -> x[1], reverse(args)))
    new{T}(vector)
  end
  function TypeDict{T}(::Val{:raw}, vector::Vector{Tuple{Type, T}}) where T
    new{T}(vector)
  end
end
function TypeDict(args::Vector{Tuple{Type, T}}) where T
  TypeDict{T}(args)
end
function TypeDict(args::Vararg{Pair{<:Type, T}}) where T
  vector = Tuple{Type, T}[(a.first, a.second) for a in args]
  TypeDict(vector)
end
function Base.copy(td::TypeDict{T}) where T
  TypeDict{T}(Val{:raw}(), copy(td._list))
end

Base.iterate(d::TypeDict, state...) = Base.iterate(d._list, state...)

function Base.setindex!(d::TypeDict{T}, value::T, key::Type) where T
  for (i, (k, v)) in enumerate(d._list)
    if k == key
      d._list[i] = (key, value)
      return d
    end
  end
  push!(d._list, (key, value))
  return d
end

function Base.getindex(d::TypeDict, key::Type)
  for (k, v) in d._list
    k == key && return v
  end
  throw(KeyError("key $key not found"))
end

function Base.haskey(d::TypeDict, key::Type)
  any(d._list) do (k, v)
    k == key
  end
end
function Base.keys(d::TypeDict)
  [x[1] for x in d._list]
end
function Base.values(d::TypeDict)
  [x[2] for x in d._list]
end
function Base.in(p::Pair{<:Type, T}, d::TypeDict{T}) where T
  p in d._list
end

# generic definitions of get and get! (don't know why they are not there already)

function Base.get(d::TypeDict{T}, key, default::T)::T where T
  if haskey(d, key)
    d[key]
  else
    default
  end
end

function Base.get(default::Union{Function,Type}, d::TypeDict, key)
  if haskey(d, key)
    d[key]
  else
    default()
  end
end
function Base.get!(d::TypeDict{T}, key, default::T)::T where T
  if haskey(d, key)
    d[key]
  else
    returnvalue = default
    d[key] = default
    returnvalue
  end
end

function Base.get!(default::Union{Function,Type}, d::TypeDict, key)
  if haskey(d, key)
    d[key]
  else
    returnvalue = default()
    d[key] = returnvalue
    returnvalue
  end
end
