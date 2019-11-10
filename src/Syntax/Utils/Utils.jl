module Utils
export iftrue, next, filtersplit,
  TypeDict,
  SortExpr, sortexpr

next(iter::Base.Iterators.Stateful) = iterate(iter)[1]

macro iftrue(expr)
  expr = esc(expr)
  quote
    $expr || return false
  end
end

filtersplit(f, a) = filter(f, a), filter(!f, a)


include("TypeDict.jl")
include("SortExpr.jl")

end  # module
