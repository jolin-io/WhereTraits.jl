using ExprParsers

# ExprParsers.to_expr(x::Union{DataType, UnionAll}) = Base.Meta.parse(repr(x))
function ExprParsers.to_expr(tv::TypeVar)
  if tv.lb === Union{} && tv.ub === Any
    tv.name
  elseif tv.lb === Union{}
    :($(tv.name) <: $(tv.ub))
  elseif tv.ub === Any
    :($(tv.name) >: $(tv.lb))
  else
    :($(tv.lb) <: $(tv.name) <: $(tv.ub))
  end
end
