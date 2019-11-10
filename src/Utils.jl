module Utils
export SortExpr, sortexpr, TypeDict, iftrue

struct SortExpr
  expr::Expr
end

macro iftrue(expr)
  expr = esc(expr)
  quote
    $expr || return false
  end
end

# Base.isless(e1::Expr, e2::Expr) = _isless_sortexpr(e1, e2)
Base.isless(e1::SortExpr, e2::SortExpr) = _isless_sortexpr(e1.expr, e2.expr)
function _isless_sortexpr(e1::Expr, e2::Expr)
  @iftrue e1.head < e2.head
  @iftrue length(e1.args) < length(e2.args)
  all(zip(e1.args, e2.args)) do (a1, a2)
    _isless_sortexpr(a1, a2)
  end
end
_isless_sortexpr(a1::T, a2::T) where T = a1 < a2
function _isless_sortexpr(l1::LineNumberNode, l2::LineNumberNode)
  l1.line < l2.line && l1.file < l2.file
end
_isless_sortexpr(a1, a2) = true

function sortexpr(a::Vector{Expr})
  [s.expr for s in sort(SortExpr.(a))]
end



struct TypeDict{T}
  _list::Vector{Pair{<:Type, <:T}}
  function TypeDict{T}() where T
    new{T}(Vector{Pair{<:Type, <:T}}())
  end
  function TypeDict{T}(args::Vector{Pair{<:Type, <:T}}) where T
    # 2x reverse because later should overwrite earlier
    vector = reverse(unique(x -> x.first, reverse(args)))
    new{T}(vector)
  end
end
function TypeDict(args::Vector{Pair{<:Type, <:T}}) where T
  TypeDict{T}(args)
end
function TypeDict(args::Vararg{Pair{<:Type, <:T}}) where T
  # 2x reverse because later should overwrite earlier
  vector = reverse(unique(x -> x.first, reverse(args)))
  vector′ = convert(Vector{Pair{<:Type, <:T}}, vector)
  TypeDict(vector′)
end

Base.iterate(d::TypeDict, state...) = Base.iterate(d._list, state...)

function Base.setindex!(d::TypeDict{T}, value::T, key::Type) where T
  for (i, (k, v)) in enumerate(d._list)
    if k == key
      d._list[i] = key => value
      return d
    end
  end
  push!(d._list, key => value)
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
  [x.first for x in d._list]
end
function Base.values(d::TypeDict)
  [x.second for x in d._list]
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

end  # module
