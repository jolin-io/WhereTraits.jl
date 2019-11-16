module Traits
export @traits, @traits_show_implementation, @traits_delete!,
  isdef, Out, NotApplicable

include("Syntax/Syntax.jl")
using .Syntax

include("BasicTraits.jl")


# in addition to the core syntax we offer two helpers to better dispatch on functions

"""
  wrapper arround Julia's type inference

This should be overloaded if you want to fix certain wrong typeinferences for
  your custom types.
Returning ``Union{}`` is interpreted as ``MethodError``.

It is used internally by both ``isdef`` and ``Out``.
"""
_return_type(f, types::Type{<:Tuple}) = Core.Compiler.return_type(f, types)


"""
  checks whether the function is defined for the actual types or not

This works in compile time and hence can be used to optimize code.

CAUTION: may lead to unepxected results when queried with AbstractTypes
Note that you can always overwrite ``Traits._return_type(f, Tuple{TypeArg1, TypeArg2, ...})``
  to fix typeinference issues for your specific types.
"""
isdef(f, types::Type{<:Tuple}) = _return_type(f, types) !== Union{}
isdef(f, types::Vararg{<:Type}) = isdef(f, Tuple{types...})
isdef(f, args...) = isdef(f, Tuple{typeof.(args)...})


struct NotApplicable end

"""
  returns outputtype of function application

Returns ``Traits.NotApplicable`` if compiler notices that no Method can be found

CAUTION: May return Any, if TypeInformation is lost by the compiler.
  Hence, if Out(...) == Any, a MethodError might happen at runtime.
Note that you can always overwrite ``Traits._return_type(f, Tuple{TypeArg1, TypeArg2, ...})``
  to fix typeinference issues for your specific types.
"""
Out(f, types::Type{<:Tuple}) = _Out(f, types, _return_type(f, types))
Out(f, types::Vararg{<:Type}) = Out(f, Tuple{types...})
Out(f, args...) = Out(f, Tuple{typeof.(args)...})
_Out(f, types, outtype::Type{Union{}}) = NotApplicable
_Out(f, types, outtype) = outtype

end # module
