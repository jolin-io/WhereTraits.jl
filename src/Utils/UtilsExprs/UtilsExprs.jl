module UtilsExprs
using Reexport

include("SortExprs.jl")
@reexport using .SortExprs

include("MacroEnvs.jl")
@reexport using .MacroEnvs

include("MacroErrors.jl")
@reexport using .MacroErrors

export depends_on, flatten_blocks, change_symbols

using ExprParsers


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



change_symbols(symbol_mapping, vec::Vector) = [change_symbols(symbol_mapping, x) for x in vec]
change_symbols(symbol_mapping, sym::Symbol) = get(symbol_mapping, sym, sym)
change_symbols(symbol_mapping, any) = any
change_symbols(symbol_mapping, qn::QuoteNode) = QuoteNode(change_symbols(symbol_mapping, qn.value))
change_symbols(symbol_mapping, expr::Expr) = Expr(expr.head, change_symbols(symbol_mapping, expr.args)...)


end  # module