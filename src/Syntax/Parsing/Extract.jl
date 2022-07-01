module Extract
export extract_vars, extract_functionnames, extract_var_from_qualified

using ExprParsers

# TODO these could be improved by using ExprParsers and going into keyword arguments 
# and further special cases

"""
if . is used, it returns the qualified name, i.e. the full 'Base.iseven'
"""
extract_vars(expr::Expr) = extract_vars(Val(expr.head), expr.args...)
extract_vars(::Val{:<:}, upperbound) = extract_vars(x)
extract_vars(::Val{:<:}, x, upperbound) = [extract_vars(x); extract_vars(upperbound)]
extract_vars(::Val{:(::)}, x) = extract_vars(x)
extract_vars(::Val{:(::)}, x, type) = [extract_vars(x); extract_vars(type)]
extract_vars(::Val{:call}, _functionname, args...) = extract_vars(args)
extract_vars(::Val{:macro}, _macroname, _linenumbernode, args...) = extract_vars(args)
extract_vars(::Val{:.}, args...) = [Expr(:., args...)]  # qualified_symbol
extract_vars(args::Union{Tuple, Vector}) = [var for arg ∈ args for var in extract_vars(arg)]
extract_vars(symbol::Symbol) = [symbol]
extract_vars(_any...) = []

"""includes macro names

if . is used, it returns the qualified name, i.e. the full 'Base.iseven'
"""
extract_functionnames(expr::Expr) = extract_functionnames(Val(expr.head), expr.args...)
extract_functionnames(::Val{:<:}, upperbound) = extract_functionnames(x)
extract_functionnames(::Val{:<:}, x, upperbound) = [extract_functionnames(x); extract_functionnames(upperbound)]
extract_functionnames(::Val{:(::)}, x) = extract_functionnames(x)
extract_functionnames(::Val{:(::)}, x, type) = [extract_functionnames(x); extract_functionnames(type)]
extract_functionnames(::Val{:call}, _functionname, args...) = [_functionname, extract_functionnames(args)...]
extract_functionnames(::Val{:macro}, _macroname, _linenumbernode, args...) = [_macroname, extract_functionnames(args)...]
extract_functionnames(args::Union{Tuple, Vector}) = [var for arg ∈ args for var in extract_functionnames(arg)]
extract_functionnames(_any...) = []


extract_var_from_qualified(symbol::Symbol) = symbol
extract_var_from_qualified(expr::Expr) = parse_expr(EP.NestedDot(), expr).properties[end]
end