module Syntax
export @traits, @traits_order

using ExprParsers
import WhereTraits
using WhereTraits: CONFIG
using WhereTraits.Utils
using WhereTraits.InternalState
using Suppressor

include("Lowering.jl")
using .Lowering

include("Parsing/Parsing.jl")
using .Parsing

include("Rendering.jl")
using .Rendering

include("Merging.jl")  # depends on Rendering
using .Merging



# @traits
# =======

"""
@traits f(a, b) where {!isempty(a), !isempty(b)} = (a[1], b[1])
"""
macro traits(expr)
    traits_impl(@MacroEnv, expr)
end

function traits_impl(env::MacroEnv, expr_original)
    expr_expanded = macroexpand(env.mod, expr_original)
    expr_traits = try
        _traits(env, expr_expanded, expr_original)
    catch exc
        isa(exc, MacroError) || rethrow()
        return :($throw($(exc.exception)))
    end
    esc(expr_traits)
end

function _traits(env, expr_expanded::Expr, expr_original::Expr)
    parser = EP.AnyOf(EP.Function(), EP.anything)
    _traits_parsed(env, parse_expr(parser, expr_expanded), expr_original)
end

function _traits_parsed(env, func_parsed::EP.Function_Parsed, expr_original::Expr)
    basefunc, lowerings = lower_args_default(func_parsed)

    basefunc_outer, basefunc_inner = parse_traitsfunction(env, basefunc, expr_original)
    _basefunc_store_new, basefunc_to_be_rendered = merge_traits(basefunc_outer, basefunc_inner, doc = true)
    exprs = [render(env, basefunc_to_be_rendered)]

    for lowering in lowerings
        # As the lowering-process dropped variables, also traits may need to be dropped. Do this silently.
        lowered_outer, lowered_inner = parse_traitsfunction(env, lowering, expr_original, on_traits_dropped = msg -> nothing)
        # we don't document lowerings
        # lowerings have another distinct signature, hence we do not reuse basefunc_store_new
        _lowering_store_new, lowering_to_be_rendered = merge_traits(lowered_outer, lowered_inner, doc = false)
        push!(exprs, render(env, lowering_to_be_rendered))
    end
    # return nothing in order to not return implementation detail
    flatten_blocks(Expr(:block, exprs..., nothing))
end

function _traits_parsed(env, parsed, expr_original)
    throw(ArgumentError("@traits macro expects function expression, got `$expr_original`"))
end




# @traits_order
# =============

"""
@traits_order AnotherModule.anotherfunc(a::T, b::T) where {T} begin
    isempty(a)
    isempty(b)
end
"""
macro traits_order(functioncall, body)
    expr_original = Expr(:block, functioncall, body)
    # for simplicity we use a function Expr as the representation
    expr = Expr(:function, functioncall, body)
    expr_expanded = macroexpand(__module__, expr)

    expr_disambiguation = _traits_order(@MacroEnv, expr_expanded, expr_original)
    expr_disambiguation = esc(expr_disambiguation)
    expr_disambiguation
end


function _traits_order(env, expr_expanded::Expr, expr_original::Expr)
    parser = EP.AnyOf(EP.Function(), EP.anything)
    _traits_order_parsed(env, parse_expr(parser, expr_expanded), expr_original)
end

function _traits_order_parsed(env, func_parsed::EP.Function_Parsed, expr_original::Expr)
    outerfunc, disambiguation = parse_traitsorder(env, func_parsed, expr_original)
    _store_new, to_be_rendered = merge_traitsorder(outerfunc, disambiguation)
    expr = render(env, to_be_rendered)
    # return nothing in order to not return implementation detail
    flatten_blocks(Expr(:block, expr, nothing))
end

function _traits_order_parsed(env, parsed, expr_original)
    throw(ArgumentError("@traits_order macro expects two arguments, first a function call expression with optional where statements, and second a begin/end block of traits. Got `$expr_original`"))
end


end # module
