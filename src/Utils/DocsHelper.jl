module DocsHelper
export @doc_signature, isnodocumentationfound, mygetdoc
using Markdown

"""
checks whether doc string is the default "No documentation found."
"""
function isnodocumentationfound(doc)
  doc isa Markdown.MD &&
  length(doc.content) >= 1 &&
  doc.content[1] isa Markdown.Paragraph &&
  length(doc.content[1].content) == 1 &&
  doc.content[1].content[1] == "No documentation found."
end


"""
    mygetdoc(binding, sig)

Like Docs.doc but returns nothing if no information is found
"""
function mygetdoc(binding::Base.Docs.Binding, sig::Type = Union{})
    if Base.Docs.defined(binding)
        result = Base.Docs.getdoc(Base.Docs.resolve(binding), sig)
        result === nothing || return result
    end
    results, groups = Base.Docs.DocStr[], Base.Docs.MultiDoc[]
    # Lookup `binding` and `sig` for matches in all modules of the docsystem.
    for mod in Base.Docs.modules
        dict = Base.Docs.meta(mod)
        if haskey(dict, binding)
            multidoc = dict[binding]
            push!(groups, multidoc)
            for msig in multidoc.order
                sig <: msig && push!(results, multidoc.docs[msig])
            end
        end
    end
    if isempty(groups)
        # When no `MultiDoc`s are found that match `binding` then we check whether `binding`
        # is an alias of some other `Binding`. When it is we then re-run `doc` with that
        # `Binding`, otherwise if it's not an alias then we generate a summary for the
        # `binding` and display that to the user instead.
        alias = Base.Docs.aliasof(binding)
        alias == binding ? nothing : mygetdoc(alias, sig)
    elseif isempty(results)
        # There was at least one match for `binding` while searching. If there weren't any
        # matches for `sig` then we don't return anything.
        nothing
    else
        # Get parsed docs and concatenate them.
        md = Base.Docs.catdoc(map(Base.Docs.parsedoc, results)...)
        # Save metadata in the generated markdown.
        if isa(md, Markdown.MD)
            md.meta[:results] = results
            md.meta[:binding] = binding
            md.meta[:typesig] = sig
        end
        return md
    end
end

# Some additional convenience `doc` methods that take objects rather than `Binding`s.
mygetdoc(obj::UnionAll) = mygetdoc(Base.unwrap_unionall(obj))
mygetdoc(object, sig::Type = Union{}) = mygetdoc(Base.Docs.aliasof(object, typeof(object)), sig)
mygetdoc(object, sig...)              = mygetdoc(object, Tuple{sig...})



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
