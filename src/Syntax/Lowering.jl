module Lowering
export lower_args_default

using ExprParsers
using Setfield
using Traits.Utils


lower_args_default(expr::Expr) = lower_args_default(parse_expr(EP.Function(), expr))
function lower_args_default(func_parsed::EP.Function_Parsed)
  args_parsed = [parse_expr(EP.Arg(), a) for a in func_parsed.args]
  defaults = [a.default for a in args_parsed]
  @assert _nodefault_only_at_front(defaults) "no normal positional argument allowed after positional argument with default value"
  count_defaults = sum(d isa EP.NoDefault ? 0 : 1 for d in defaults)

  # collect several expressions

  # the whole function is returned without defaults
  args = [to_expr(@set a.default = EP.nodefault) for a in args_parsed]
  func_without_defaults = @set func_parsed.args = args
  # collect all lowerings
  lowerings = []
  for i in 1:count_defaults  # 1:0 is empty
    symbols = [a.name for a in args_parsed[1:end-i]]
    dropped_symbols = [a.name for a in args_parsed[end-i+1:end]]
    defaults = [a.default for a in args_parsed[end-i+1:end]]
    drop_unapplicable_traits(wheres) = filter(wheres) do expr
      !depends_on(expr, dropped_symbols)
    end

    lowering = EP.Function_Parsed(
      name = func_parsed.name,
      curlies = copy(func_parsed.curlies),
      args = [to_expr(@set a.default = EP.nodefault) for a in args_parsed[1:end-i]],
      kwargs = [],
      # the traits normalization kicks out all unused wheres where TypeVariables matter
      # but dependencies on args still need to be handled
      wheres = drop_unapplicable_traits(func_parsed.wheres),
      body = Expr(:call, func_parsed.name, symbols..., defaults...),
    )
    push!(lowerings, lowering)
  end
  func_without_defaults, lowerings
end


function _nodefault_only_at_front(defaults::Vector)
  if isempty(defaults)
    true
  else
    first = defaults[1]
    if first isa EP.NoDefault
      _nodefault_only_at_front(defaults[2:end])
    else
      _no_nodefault_any_more(defaults[2:end])
    end
  end
end

function _no_nodefault_any_more(defaults::Vector)
  if isempty(defaults)
    true
  else
    first = defaults[1]
    if first isa EP.NoDefault
      false
    else
      _no_nodefault_any_more(defaults[2:end])
    end
  end
end

end # module
