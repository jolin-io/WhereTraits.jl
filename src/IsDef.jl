module IsDef
export isdef, Out, NotApplicable, @create_newtype
import Traits
using ASTParser

# in addition to the core syntax we offer two helpers to better dispatch on functions

"""
  wrapper arround Julia's type inference

This should be overloaded if you want to fix certain wrong typeinferences for
  your custom types.
Returning ``Union{}`` is interpreted as ``MethodError``.

It is used internally by both ``isdef`` and ``Out``.
"""
function _return_type(f, types::Type{<:Tuple})
  Core.Compiler.return_type(f, newtype_signature(types))
end


"""
  checks whether the function is defined for the actual types or not

This works in compile time and hence can be used to optimize code.

IMPORTANT: Overload ``Traits.IsDef._return_type`` if you experience unexpected behaviour for your types
For instance to say that some call like ``myfunc(::Int, ::String)`` is not defined define the following
```julia
function Traits.IsDef._return_type(::Typeof(myfunc), ::Type{Tuple{TypeArg1, TypeArg2}})
  Union{}  # return empty Union to indicate something is not defined
end
```
"""
isdef(f, types::Type{<:Tuple}) = _return_type(f, types) !== Union{}
isdef(f, types::Vararg{<:Type}) = isdef(f, Tuple{types...})
isdef(f, args...) = isdef(f, Tuple{typeof.(args)...})


struct NotApplicable end

"""
  returns outputtype of function application

Returns ``Traits.NotApplicable`` if compiler notices that no Method can be found

CAUTION: If ``Out(...) == Any``, still a MethodError might happen at runtime. This is due to incomplete type inference.

SOLUTION: Overload ``Traits.IsDef._return_type`` if you experience unexpected behaviour for your types
For instance to say that some call like ``myfunc(::Int, ::String)`` is not defined define the following
```julia
function Traits.IsDef._return_type(::typeof(myfunc), ::Type{Tuple{TypeArg1, TypeArg2}})
  Union{}  # return empty Union to indicate something is not defined
end
```
"""
Out(f, types::Type{<:Tuple}) = _Out(f, types, _return_type(f, types))
Out(f, types::Vararg{<:Type}) = Out(f, Tuple{types...})
Out(f, args...) = Out(f, Tuple{typeof.(args)...})
_Out(f, types, outtype::Type{Union{}}) = NotApplicable
_Out(f, types, outtype) = outtype


@generated function newtype_signature(::Type{T}) where T <: Tuple
  Tuple{_newtype.(T.parameters)...}
end

# we need a small intermediate function to conveniently work with UnionAll types
_newtype(T) = newtype(T)
function _newtype(T::UnionAll)
  # get generic newtype based on standardized UnionAll type
  S = newtype(Base.unwrap_unionall(T).name.wrapper)
  # union types are mapped to union of respective newtypes
  rewrap_unionall(S, T)
end

function newtype(T::Type)
  if !isabstracttype(T)
    T
  else
    @error """
      Cannot find a newtype for abstract type $T.
      Please run ``Traits.@create_newtype $T``.

      Or ``Traits.@create_newtype AbstractType1 AbstractType2 ...`` if you have several new abstract types.
    """
  end
end
function newtype(T::Union)
  # union types are mapped to union of respective newtypes
  Union{newtype(t.a), newtype(t.b)}
end

macro create_newtype(typename)
  if typename isa Expr && typename.head == :block
    esc(create_newtype(__module__, typename.args))
  else
    esc(create_newtype(__module__, typename))
  end
end
macro create_newtype(typenames...)
  esc(create_newtype(__module__, typenames))
end


function create_newtype(__module__, typename)
  type = Base.eval(__module__, typename)
  base, typevars = split_where(type)
  newtype_name = Symbol("'", "__newtype__.", type, "'")

  if isdefined(__module__, newtype_name)
    nothing
  elseif isempty(typevars)
    quote
      struct $newtype_name <: $type end
      function Traits.IsDef.newtype(::Type{$type})
        $newtype_name
      end
    end
  else
    typevars_symbols = Symbol.(typevars)
    quote
      struct $newtype_name{$(typevars_symbols...)} <: $type{$(typevars_symbols...)} end
      # map generic unionall type (used to reconstruct also all partly union-all types)
      function Traits.IsDef.newtype(::Type{$type})
        $newtype_name
      end
      # map on concrete datatypes
      function Traits.IsDef.newtype(::Type{$type{$(typevars_symbols...)}}) where {$(typevars_symbols...)}
        $newtype_name{$(typevars_symbols...)}
      end
    end
  end
end

function create_newtype(__module__, typenames::Union{Vector, Tuple})
  expr = Expr(:block)
  for typename in typenames
    push!(expr.args, create_newtype(__module__, typename))
  end
  expr
end

function split_where(T::UnionAll)
  base, typevars = split_where(T.body)
  base, [T.var; typevars...]
end
split_where(T) = T, []

function rewrap_unionall(newtype, target)
  # we want to handle also fixed typevars, hence we use parameters
  target_base, target_typevars = split_where(target)
  target_typeargs = target_base.parameters

  newtype = newtype{target_typeargs...}
  for typevar in target_typevars
    newtype = UnionAll(typevar, newtype)
  end
  newtype
end

supertypes(::Type{Any}) = [Any]
supertypes(T::Type) = [T; supertypes(supertype(T))]


# some known abstracttypes from Core and  Base
# ----------------------------------
@create_newtype Any
@create_newtype Signed Integer Real Number
@create_newtype AbstractFloat
@create_newtype AbstractChar AbstractString
@create_newtype AbstractDict
@create_newtype DenseArray AbstractArray AbstractVector AbstractMatrix Base.AbstractZeroDimArray
@create_newtype AbstractUnitRange OrdinalRange AbstractRange
@create_newtype AbstractSet
@create_newtype(
  Base.AbstractCartesianIndex, Base.AbstractChannel, Base.AbstractCmd,
  Base.AbstractDisplay, Base.AbstractIrrational, Base.AbstractLock,
  Base.AbstractPipe)

end # module
