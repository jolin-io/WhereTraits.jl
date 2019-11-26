struct SortExpr
  expr::Any
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

function sortexpr(a::Vector)
  [s.expr for s in sort(SortExpr.(a))]
end
