module Syntax
export @traits, @traits_test, @traits_show_implementation

using ExprParsers
import Traits
using Traits: CONFIG
using Traits.Utils
using Traits.InternalState
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
  store::Traits.InternalState.TraitsStore = getorcreate_traitsstore(env.mod, func_parsed.name)
  basefunc, lowerings = lower_args_default(func_parsed)
  basefunc_outer, basefunc_inner = parse_traitsfunction(env, basefunc, expr_original)

  # we need to split merging of the new function into the existing store
  # and rendering the new updates, as the st
  store, exprs = merge_and_render_function(env, store, basefunc_outer, basefunc_inner)

  for lowering in lowerings
    # As lowering dropped variables, also traits may need to be dropped. Do this silently.
    lowered_outer, lowered_inner = parse_traitsfunction(env, lowering, expr_original, on_traits_dropped = msg -> nothing)
    # we don't document lowerings
    store, lowered_exprs = merge_and_render_function(env, store, lowered_outer, lowered_inner, doc = false)
    append!(exprs, lowered_exprs)
  end
  # finally render the new store
  push!(exprs, render_store_update(env, store))
  # return nothing in order to not return implementation detail
  flatten_blocks(Expr(:block, exprs..., nothing))
end

function _traits_parsed(env, parsed, expr_original)
  throw(ArgumentError("@traits macro expects function expression, got ``$expr_original``"))
end

# # TODO should we Deprecate this syntax?
# function _traits(mod, block_parsed::EP.Block_Parsed)
#   # @traits on block doesn't use any global state
#   store = TraitsStore()
#   parser = EP.AnyOf(EP.Function(), EP.anything)
#   funcs = [p for p in [parser(a) for a in block_parsed.exprs] if p isa EP.Function]
#   for f in funcs
#     outerfunc, innerfunc = parse_traitsfunction(mod, f)
#     merge!(store, outerfunc, innerfunc)
#   end
#   render(store)
# end
end # module
