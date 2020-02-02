module DocsHelper
export @doc_signature

"""
Base.Docs.@doc cannot parse more signatures directly, but only if a true function is defined

Hence we do our own version by using the code which is normally used to extract the signature out of
function definitions.
"""
macro doc_signature(doc, expr)
  Docs.objectdoc(__source__, __module__, doc, :(nothing), expr, signature(expr))
end

# TODO open a ticket for this change that tv are not added separately as signatures:
function signature!(tv, expr::Expr)
    is_macrocall = Docs.isexpr(expr, :macrocall)
    if is_macrocall || Docs.isexpr(expr, :call)
        sig = :(Union{Tuple{}})
        first_arg = is_macrocall ? 3 : 2 # skip function arguments
        for arg in expr.args[first_arg:end]
            Docs.isexpr(arg, :parameters) && continue
            if Docs.isexpr(arg, :kw) # optional arg
                push!(sig.args, :(Tuple{$(sig.args[end].args[2:end]...)}))
            end
            push!(sig.args[end].args, Docs.argtype(arg))
        end
        if Docs.isexpr(expr.args[1], :curly) && isempty(tv)
            append!(tv, tvar.(expr.args[1].args[2:end]))
        end
        # for i = length(tv):-1:1
        #     push!(sig.args, :(Tuple{$(tv[i].args[1])}))
        # end
        for i = length(tv):-1:1
            sig = Expr(:where, sig, tv[i])
        end
        return sig
    elseif Docs.isexpr(expr, :where)
        append!(tv, Docs.tvar.(expr.args[2:end]))
        return signature!(tv, expr.args[1])
    else
        return signature!(tv, expr.args[1])
    end
end
signature!(tv, @nospecialize(other)) = :(Union{})
signature(expr::Expr) = signature!([], expr)
signature(@nospecialize other) = signature!([], other)

end # module
