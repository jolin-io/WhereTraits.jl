module Syntax
export @traits, @traits_show_implementation

using ExprParsers
import WhereTraits
using WhereTraits: CONFIG
using WhereTraits.Utils
using WhereTraits.InternalState
using Suppressor
include("Lowering.jl")
using .Lowering
include("Parsing.jl")
using .Parsing
include("Rendering.jl")
using .Rendering

"""
@traits f(a, b) where {!isempty(a), !isempty(b)} = (a[1], b[1])
"""
macro traits(expr_original)
  expr_expanded = macroexpand(__module__, expr_original)
  expr_traits = _traits(@MacroEnv, expr_expanded, expr_original)
  expr_traits = esc(expr_traits)
  if CONFIG.suppress_on_traits_definitions
    expr_traits = :(@suppress $expr_traits)
  end
  expr_traits
end

function _traits(env, expr_expanded::Expr, expr_original::Expr)
  parser = EP.AnyOf(EP.Function(), EP.anything)
  _traits_parsed(env, parse_expr(parser, expr_expanded), expr_original)
end

function _traits_parsed(env, func_parsed::EP.Function_Parsed, expr_original::Expr)
  basefunc, lowerings = lower_args_default(func_parsed)

  basefunc_outer, basefunc_inner = parse_traitsfunction(env, basefunc, expr_original)
  exprs = merge_and_render_update(env, basefunc_outer, basefunc_inner, doc = true)

  for lowering in lowerings
    # As lowering dropped variables, also traits may need to be dropped. Do this silently.
    lowered_outer, lowered_inner = parse_traitsfunction(env, lowering, expr_original, on_traits_dropped = msg -> nothing)
    # we don't document lowerings
    lowered_exprs = merge_and_render_update(env, lowered_outer, lowered_inner, doc = false)
    append!(exprs, lowered_exprs)
  end
  # return nothing in order to not return implementation detail
  flatten_blocks(Expr(:block, exprs..., nothing))
end

function _traits_parsed(env, parsed, expr_original)
  throw(ArgumentError("@traits macro expects function expression, got `$expr_original`"))
end

end # module
