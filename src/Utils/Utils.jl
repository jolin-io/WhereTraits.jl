module Utils
export iftrue, next!, filtersplit, depends_on,
  normalize_mod_and_name, unique_funcname,
  flatten_blocks, MacroEnv, @MacroEnv, Dict_to_normalizedType,
  TypeDict,
  SortExpr, sortexpr,
  @doc_signature, isnodocumentationfound

using ExprParsers

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


# flatten out blocks
flatten_blocks(expr::Expr) = flatten_blocks(Val{expr.head}(), expr.args)
flatten_blocks(any) = any
flatten_blocks(::Val{head}, args) where head = Expr(head, flatten_blocks.(args)...)
function flatten_blocks(head::Val{:block}, args)
  args′ = flatten_blocks.(args)
  args′′ = [((a isa Expr && a.head == :block) ? a.args : [a] for a in args′)...;]
  Expr(:block, args′′...)
end


@Base.kwdef struct MacroEnv
  source::LineNumberNode
  mod::Module
end

macro MacroEnv()
  quote
    MacroEnv($(esc(:__source__)), $(esc(:__module__)))
  end
end

function Dict_to_normalizedType(d::AbstractDict)
  # TODO performance improvement possible - Symbol is called once for sorting, and once for conversion, could be combined
  ks = d |> keys |> collect |> a -> sort!(a, by=Symbol)
  rows = [Pair{Symbol(k), Symbol(d[k])} for k in ks]
  Tuple{rows...}
end

function normalize_mod_and_name(mod, name)
  parser = EP.AnyOf(EP.anysymbol, EP.NestedDot())
  normalize_mod_and_name(mod, parse_expr(parser, name))
end
normalize_mod_and_name(mod, name::Symbol) = mod, name
function normalize_mod_and_name(mod, name::EP.NestedDot_Parsed)
  mod′::Module = getproperty(mod, name.base)
  for field in name.properties[1:end-1]
    mod′ = getproperty(mod′, field)
  end
  mod′, name.properties[end]  # NestedDot.properties is known to be non-empty
end
function unique_funcname(mod, funcname)
  mod′, funcname′ = normalize_mod_and_name(mod, funcname)
  Symbol(mod′, :., funcname′)
end


include("exprparsers.jl")
include("TypeDict.jl")
include("SortExpr.jl")
include("DocsHelper.jl")
using .DocsHelper

end  # module
