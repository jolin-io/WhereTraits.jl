module Utils
export iftrue, next!, filtersplit, depends_on,
  TypeDict,
  SortExpr, sortexpr,
  @doc_signature, isnodocumentationfound

next!(iter::Base.Iterators.Stateful) = iterate(iter)[1]

macro iftrue(expr)
  expr = esc(expr)
  quote
    $expr || return false
  end
end

filtersplit(f, a) = filter(f, a), filter(!f, a)

""" check whether a certain Expr builds upon given symbols """
depends_on(expr::Base.Expr, symbols) = depends_on(expr.args, symbols)
depends_on(args::Vector, symbols) = any(a -> depends_on(a, symbols), args)
depends_on(symbol::Base.Symbol, symbols) = symbol in symbols
depends_on(any, symbols) = false

include("TypeDict.jl")
include("SortExpr.jl")
include("DocsHelper.jl")
using .DocsHelper

end  # module
