# WhereTraits Details

## Auto Documentation

The package supports advanced auto-documentation which gives you a good overview about what is defined in a `@traits` function.

Let's consider the following example
```julia
@traits h(a) where {eltype(a) <: Number} = true
@traits h(a) = false
h([1.0]) # true
h([""]) # false
```

then you get nice formatted documentation

```julia
help?> h

  h(a1::T1; kwargs...) where T1

  ––– Original @traits definitions follow –––

  h(a) where eltype(a) <: Number

  Original @traits definition:

  (h(a) where eltype(a) <: Number) = begin
          #= none:1 =#
          true
      end

    •      •      •  

  h(a)

  Original @traits definition:

  h(a) = begin
          #= none:1 =#
          false
      end
```


## MethodError handling

Like with normal function dispatch you can also get MethodErrors when using traits. 
While the pure MethodError can be helpful for the experienced WhereTraits user, for most
it will be rather incomprehensible. This is why WhereTraits catches MethodErrors and 
rephrases them in easier to understand `TraitsMethodError`.

Here one example
```julia
julia> @traits myfunc(a) where {Base.IteratorSize(a)::Base.HasShape} = 2

julia> myfunc([1])
2

julia> myfunc(Iterators.countfrom())
ERROR: TraitsMethodError: no method matching `myfunc(Base.Iterators.Count{Int64}(1, 1))`.

It corresponds to the normal julia-dispatch (aka "outerfunction")

```julia
myfunc(a1::T1; kwargs...) where T1
```

and the traits (traits are normalized to <:)

```julia
Core.Typeof(Base.IteratorSize(a1)) <: Base.IsInfinite
```

however, the only available traits definitions are:

  *   *   *   *   *   *   *   *   *   *   *   *   *   *

```julia
myfunc(a) where Base.IteratorSize(a)::Base.HasShape
```

which defines the traits (traits are normalized to <:)

```julia
Core.Typeof(Base.IteratorSize(a1)) <: Base.HasShape
```

  *   *   *   *   *   *   *   *   *   *   *   *   *   *

Note the following `<:` standardization of traits:

|        WhereTraits |                               Example |                                   `<:` standardization |
| ------------------:| -------------------------------------:| ------------------------------------------------------:|
|         bool trait |                           `iseven(a)` |  `WhereTraits.BoolType(iseven(a)) <: WhereTraits.True` |
| negated bool trait |                          `!iseven(a)` | `WhereTraits.BoolType(iseven(a)) <: WhereTraits.False` |
|        `isa` trait | `Base.IteratorSize(a)::Base.HasShape` |   `Core.Typeof(Base.IteratorSize(a)) <: Base.HasShape` |
|         `<:` trait |            `Base.eltype(a) <: Number` |                             `Base.eltype(a) <: Number` |

Stacktrace:
 [1] #myfunc#4
   @ ~/.julia/dev/WhereTraits/src/Syntax/Rendering.jl:215 [inlined]
 [2] myfunc(a1::Base.Iterators.Count{Int64}; kwargs::Base.Pairs{Symbol, Union{}, Tuple{}, NamedTuple{(), Tuple{}}})
   @ Main ./REPL[10]:0
 [3] myfunc
   @ ~/.julia/dev/WhereTraits/src/Syntax/Rendering.jl:211 [inlined]
 [4] myfunc(a1::Base.Iterators.Count{Int64})
   @ Main ./REPL[10]:0
 [5] top-level scope
   @ REPL[12]:1

caused by: MethodError: no method matching myfunc(::WhereTraits.InternalState.TraitsDefSingleton, ::Type{Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1}, ::Base.Iterators.Count{Int64}, ::<ARGS|TYPEVARS>, ::Type{Base.Iterators.Count{Int64}}, ::<TYPEVARS|TRAITS>, ::Type{Base.IsInfinite})┌ Error: Error showing method candidates, aborted
│   exception =
│    could not determine location of method definition
│    Stacktrace:
│      [1] error(s::String)
│        @ Base ./error.jl:33
│      [2] functionloc
│        @ ./methodshow.jl:164 [inlined]
│      [3] show_method_candidates(io::IOContext{Base.TTY}, ex::MethodError, kwargs::Any)
│        @ Base ./errorshow.jl:499
│      [4] showerror(io::IOContext{Base.TTY}, ex::MethodError)
│        @ Base ./errorshow.jl:318
│      [5] showerror(io::IOContext{Base.TTY}, ex::MethodError, bt::Vector{Base.StackTraces.StackFrame}; backtrace::Bool)
│        @ Base ./errorshow.jl:88
│      [6] show_exception_stack(io::IOContext{Base.TTY}, stack::Vector{Any})
│        @ Base ./errorshow.jl:866
│      [7] display_error(io::IOContext{Base.TTY}, stack::Base.ExceptionStack)
│        @ Base ./client.jl:104
│      [8] #invokelatest#2
│        @ ./essentials.jl:716 [inlined]
│      [9] invokelatest
│        @ ./essentials.jl:714 [inlined]
│     [10] print_response(errio::IO, response::Any, show_value::Bool, have_color::Bool, specialdisplay::Union{Nothing, AbstractDisplay})
│        @ REPL /nix/store/qmlbjq46nhn1v08nnb23y2myrvra8hm7-julia-bin-1.7.1/share/julia/stdlib/v1.7/REPL/src/REPL.jl:286
│     [11] (::REPL.var"#45#46"{REPL.LineEditREPL, Pair{Any, Bool}, Bool, Bool})(io::Any)
│        @ REPL /nix/store/qmlbjq46nhn1v08nnb23y2myrvra8hm7-julia-bin-1.7.1/share/julia/stdlib/v1.7/REPL/src/REPL.jl:275
│     [12] with_repl_linfo(f::Any, repl::REPL.LineEditREPL)
│        @ REPL /nix/store/qmlbjq46nhn1v08nnb23y2myrvra8hm7-julia-bin-1.7.1/share/julia/stdlib/v1.7/REPL/src/REPL.jl:508
│     [13] print_response(repl::REPL.AbstractREPL, response::Any, show_value::Bool, have_color::Bool)
│        @ REPL /nix/store/qmlbjq46nhn1v08nnb23y2myrvra8hm7-julia-bin-1.7.1/share/julia/stdlib/v1.7/REPL/src/REPL.jl:273
│     [14] (::REPL.var"#do_respond#66"{Bool, Bool, REPL.var"#77#87"{REPL.LineEditREPL, REPL.REPLHistoryProvider}, REPL.LineEditREPL, REPL.LineEdit.Prompt})(s::REPL.LineEdit.MIState, buf::Any, ok::Bool)
│        @ REPL /nix/store/qmlbjq46nhn1v08nnb23y2myrvra8hm7-julia-bin-1.7.1/share/julia/stdlib/v1.7/REPL/src/REPL.jl:844
│     [15] #invokelatest#2
│        @ ./essentials.jl:716 [inlined]
│     [16] invokelatest
│        @ ./essentials.jl:714 [inlined]
│     [17] run_interface(terminal::REPL.Terminals.TextTerminal, m::REPL.LineEdit.ModalInterface, s::REPL.LineEdit.MIState)
│        @ REPL.LineEdit /nix/store/qmlbjq46nhn1v08nnb23y2myrvra8hm7-julia-bin-1.7.1/share/julia/stdlib/v1.7/REPL/src/LineEdit.jl:2493
│     [18] run_frontend(repl::REPL.LineEditREPL, backend::REPL.REPLBackendRef)
│        @ REPL /nix/store/qmlbjq46nhn1v08nnb23y2myrvra8hm7-julia-bin-1.7.1/share/julia/stdlib/v1.7/REPL/src/REPL.jl:1230
│     [19] (::REPL.var"#49#54"{REPL.LineEditREPL, REPL.REPLBackendRef})()
│        @ REPL ./task.jl:423
└ @ Base errorshow.jl:320

Stacktrace:
 [1] myfunc(::WhereTraits.InternalState.TraitsDisambiguationSingleton, ::Type{Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1}, a1::Base.Iterators.Count{Int64}, ::<ARGS|TYPEVARS>, T1::Type, ::<TYPEVARS|TRAITS>, 'trait_1_Base.IteratorSize(a1)'::Type; kwargs::Base.Pairs{Symbol, Union{}, Tuple{}, NamedTuple{(), Tuple{}}})
   @ Main ./none:0
 [2] myfunc(::WhereTraits.InternalState.TraitsDisambiguationSingleton, ::Type{Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1}, a1::Base.Iterators.Count{Int64}, ::<ARGS|TYPEVARS>, T1::Type, ::<TYPEVARS|TRAITS>, 'trait_1_Base.IteratorSize(a1)'::Type)
   @ Main ./none:0
 [3] #myfunc#4
   @ ~/.julia/dev/WhereTraits/src/Syntax/Rendering.jl:212 [inlined]
 [4] myfunc(a1::Base.Iterators.Count{Int64}; kwargs::Base.Pairs{Symbol, Union{}, Tuple{}, NamedTuple{(), Tuple{}}})
   @ Main ./REPL[10]:0
 [5] myfunc
   @ ~/.julia/dev/WhereTraits/src/Syntax/Rendering.jl:211 [inlined]
 [6] myfunc(a1::Base.Iterators.Count{Int64})
   @ Main ./REPL[10]:0
 [7] top-level scope
   @ REPL[12]:1
```

The error now states clearly that the demanded trait `Core.Typeof(Base.IteratorSize(a1)) <: Base.IsInfinite` is not defined yet.
Internally within WhereTraits all traits (bool traits, `::`-traits and `<:`-traits) are normalized to `<:`-traits.

To fix this implementation you would add a definition with the trait `Base.IteratorSize(a1)::Base.IsInfinite`.

------------------

A second example is about a typo: Here the value-level `::` trait is confused with the type-level `<:` Trait.

```julia
julia> @traits myfunc2(a) where {Base.IteratorSize(a) <: Base.HasShape} = 2  # wrong!! it should be `Base.IteratorSize(a)::Base.HasShape`

julia> myfunc2([1])
ERROR: TraitsMethodError: no method matching `myfunc2([1])`.

It corresponds to the normal julia-dispatch (aka "outerfunction")

```julia
myfunc2(a1::T1; kwargs...) where T1
```

and the traits (traits are normalized to <:)

```julia
Base.IteratorSize(a1) <: Base.HasShape{1}()
```

however, the only available traits definitions are:

  *   *   *   *   *   *   *   *   *   *   *   *   *   *

```julia
myfunc2(a) where Base.IteratorSize(a) <: Base.HasShape
```

which defines the traits (traits are normalized to <:)

```julia
Base.IteratorSize(a1) <: Base.HasShape
```

  *   *   *   *   *   *   *   *   *   *   *   *   *   *

Note the following `<:` standardization of traits:

|        WhereTraits |                               Example |                                   `<:` standardization |
| ------------------:| -------------------------------------:| ------------------------------------------------------:|
|         bool trait |                           `iseven(a)` |  `WhereTraits.BoolType(iseven(a)) <: WhereTraits.True` |
| negated bool trait |                          `!iseven(a)` | `WhereTraits.BoolType(iseven(a)) <: WhereTraits.False` |
|        `isa` trait | `Base.IteratorSize(a)::Base.HasShape` |   `Core.Typeof(Base.IteratorSize(a)) <: Base.HasShape` |
|         `<:` trait |            `Base.eltype(a) <: Number` |                             `Base.eltype(a) <: Number` |

Stacktrace:
 [1] #myfunc2#6
   @ ~/.julia/dev/WhereTraits/src/Syntax/Rendering.jl:215 [inlined]
 [2] myfunc2(a1::Vector{Int64}; kwargs::Base.Pairs{Symbol, Union{}, Tuple{}, NamedTuple{(), Tuple{}}})
   @ Main ./REPL[13]:0
 [3] myfunc2
   @ ~/.julia/dev/WhereTraits/src/Syntax/Rendering.jl:211 [inlined]
 [4] myfunc2(a1::Vector{Int64})
   @ Main ./REPL[13]:0
 [5] top-level scope
   @ REPL[14]:1

caused by: MethodError: no method matching myfunc2(::WhereTraits.InternalState.TraitsDefSingleton, ::Type{Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1}, ::Vector{Int64}, ::<ARGS|TYPEVARS>, ::Type{Vector{Int64}}, ::<TYPEVARS|TRAITS>, ::Base.HasShape{1})┌ Error: Error showing method candidates, aborted
│   exception =
│    could not determine location of method definition
│    Stacktrace:
│      [1] error(s::String)
│        @ Base ./error.jl:33
│      [2] functionloc
│        @ ./methodshow.jl:164 [inlined]
│      [3] show_method_candidates(io::IOContext{Base.TTY}, ex::MethodError, kwargs::Any)
│        @ Base ./errorshow.jl:499
│      [4] showerror(io::IOContext{Base.TTY}, ex::MethodError)
│        @ Base ./errorshow.jl:318
│      [5] showerror(io::IOContext{Base.TTY}, ex::MethodError, bt::Vector{Base.StackTraces.StackFrame}; backtrace::Bool)
│        @ Base ./errorshow.jl:88
│      [6] show_exception_stack(io::IOContext{Base.TTY}, stack::Vector{Any})
│        @ Base ./errorshow.jl:866
│      [7] display_error(io::IOContext{Base.TTY}, stack::Base.ExceptionStack)
│        @ Base ./client.jl:104
│      [8] #invokelatest#2
│        @ ./essentials.jl:716 [inlined]
│      [9] invokelatest
│        @ ./essentials.jl:714 [inlined]
│     [10] print_response(errio::IO, response::Any, show_value::Bool, have_color::Bool, specialdisplay::Union{Nothing, AbstractDisplay})
│        @ REPL /nix/store/qmlbjq46nhn1v08nnb23y2myrvra8hm7-julia-bin-1.7.1/share/julia/stdlib/v1.7/REPL/src/REPL.jl:286
│     [11] (::REPL.var"#45#46"{REPL.LineEditREPL, Pair{Any, Bool}, Bool, Bool})(io::Any)
│        @ REPL /nix/store/qmlbjq46nhn1v08nnb23y2myrvra8hm7-julia-bin-1.7.1/share/julia/stdlib/v1.7/REPL/src/REPL.jl:275
│     [12] with_repl_linfo(f::Any, repl::REPL.LineEditREPL)
│        @ REPL /nix/store/qmlbjq46nhn1v08nnb23y2myrvra8hm7-julia-bin-1.7.1/share/julia/stdlib/v1.7/REPL/src/REPL.jl:508
│     [13] print_response(repl::REPL.AbstractREPL, response::Any, show_value::Bool, have_color::Bool)
│        @ REPL /nix/store/qmlbjq46nhn1v08nnb23y2myrvra8hm7-julia-bin-1.7.1/share/julia/stdlib/v1.7/REPL/src/REPL.jl:273
│     [14] (::REPL.var"#do_respond#66"{Bool, Bool, REPL.var"#77#87"{REPL.LineEditREPL, REPL.REPLHistoryProvider}, REPL.LineEditREPL, REPL.LineEdit.Prompt})(s::REPL.LineEdit.MIState, buf::Any, ok::Bool)
│        @ REPL /nix/store/qmlbjq46nhn1v08nnb23y2myrvra8hm7-julia-bin-1.7.1/share/julia/stdlib/v1.7/REPL/src/REPL.jl:844
│     [15] #invokelatest#2
│        @ ./essentials.jl:716 [inlined]
│     [16] invokelatest
│        @ ./essentials.jl:714 [inlined]
│     [17] run_interface(terminal::REPL.Terminals.TextTerminal, m::REPL.LineEdit.ModalInterface, s::REPL.LineEdit.MIState)
│        @ REPL.LineEdit /nix/store/qmlbjq46nhn1v08nnb23y2myrvra8hm7-julia-bin-1.7.1/share/julia/stdlib/v1.7/REPL/src/LineEdit.jl:2493
│     [18] run_frontend(repl::REPL.LineEditREPL, backend::REPL.REPLBackendRef)
│        @ REPL /nix/store/qmlbjq46nhn1v08nnb23y2myrvra8hm7-julia-bin-1.7.1/share/julia/stdlib/v1.7/REPL/src/REPL.jl:1230
│     [19] (::REPL.var"#49#54"{REPL.LineEditREPL, REPL.REPLBackendRef})()
│        @ REPL ./task.jl:423
└ @ Base errorshow.jl:320

Stacktrace:
 [1] myfunc2(::WhereTraits.InternalState.TraitsDisambiguationSingleton, ::Type{Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1}, a1::Vector{Int64}, ::<ARGS|TYPEVARS>, T1::Type, ::<TYPEVARS|TRAITS>, 'trait_1_Base.IteratorSize(a1)'::Base.HasShape{1}; kwargs::Base.Pairs{Symbol, Union{}, Tuple{}, NamedTuple{(), Tuple{}}})
   @ Main ./none:0
 [2] myfunc2(::WhereTraits.InternalState.TraitsDisambiguationSingleton, ::Type{Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1}, a1::Vector{Int64}, ::<ARGS|TYPEVARS>, T1::Type, ::<TYPEVARS|TRAITS>, 'trait_1_Base.IteratorSize(a1)'::Base.HasShape{1})
   @ Main ./none:0
 [3] #myfunc2#6
   @ ~/.julia/dev/WhereTraits/src/Syntax/Rendering.jl:212 [inlined]
 [4] myfunc2(a1::Vector{Int64}; kwargs::Base.Pairs{Symbol, Union{}, Tuple{}, NamedTuple{(), Tuple{}}})
   @ Main ./REPL[13]:0
 [5] myfunc2
   @ ~/.julia/dev/WhereTraits/src/Syntax/Rendering.jl:211 [inlined]
 [6] myfunc2(a1::Vector{Int64})
   @ Main ./REPL[13]:0
 [7] top-level scope
   @ REPL[14]:1
```




## Implementation Details

The implementations uses only code-rewrite, creating two nested functions out of the one `@traits` function.
The outer function dispatches as normal, the inner function dispatches on the added traits functionality.

To inspect what is going on it is helpful to turn off the auto_documentation feature by setting
```julia
WhereTraits.CONFIG.auto_documentation = false
```
After this the macroexpand is simpler to understand
```julia
@macroexpand @traits foo(a) where {isodd(a)} = (a+1)/2
```
which gives the following code
```julia
function foo(a1::T1; kwargs...) where T1
    #= REPL[6]:1 =#
    #= /home/ssahm/.julia/dev/WhereTraits/src/Syntax/Rendering.jl:211 =#
    try
        #= /home/ssahm/.julia/dev/WhereTraits/src/Syntax/Rendering.jl:212 =#
        (Main).foo(WhereTraits.InternalState.TraitsDisambiguationSingleton(), Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1, a1, <ARGS|TYPEVARS>(), T1, <TYPEVARS|TRAITS>(), (WhereTraits.Utils.BoolTypes.BoolType)(isodd(a1)); kwargs...)
    catch exc
        #= /home/ssahm/.julia/dev/WhereTraits/src/Syntax/Rendering.jl:214 =#
        (WhereTraits.InternalState.isWhereTraitsMethodError)(exc) || rethrow()
        #= /home/ssahm/.julia/dev/WhereTraits/src/Syntax/Rendering.jl:215 =#
        throw((WhereTraitsMethodError)(exc))
    end
end
function foo(::WhereTraits.InternalState.TraitsDefSingleton, ::Type{Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1}, a, ::<ARGS|TYPEVARS>, var"'T1'"::Any, ::<TYPEVARS|TRAITS>, var"'isodd(a1)'"::Type{<:WhereTraits.Utils.BoolTypes.True})
    #= REPL[6]:1 =#
    (a + 1) / 2
end
function foo(::WhereTraits.InternalState.TraitsDisambiguationSingleton, ::Type{Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1}, a1, ::<ARGS|TYPEVARS>, T1, ::<TYPEVARS|TRAITS>, var"'trait_1_isodd(a1)'"::Any; kwargs...)
    (Main).foo(WhereTraits.InternalState.TraitsDefSingleton(), Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1, a1, <ARGS|TYPEVARS>(), T1, <TYPEVARS|TRAITS>(), var"'trait_1_isodd(a1)'"; kwargs...)
end
nothing
function foo(::WhereTraits.InternalState.TraitsStoreSingleton, ::Type{Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1})
    #= /home/ssahm/.julia/dev/WhereTraits/src/Syntax/Rendering.jl:76 =#
    #= /home/ssahm/.julia/dev/WhereTraits/src/Syntax/Rendering.jl:77 =#
    WhereTraits.InternalState.TraitsStore{Tuple{<CURLIES|SIGNATURE|ARGS>, Any}}(WhereTraits.InternalState.DefOuterFunc{Tuple{<CURLIES|SIGNATURE|ARGS>, Any}}(WhereTraits.InternalState.DefOuterFuncFixedPart{Tuple{<CURLIES|SIGNATURE|ARGS>, Any}}(Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1, Main, :foo, Union{Expr, Symbol}[], ExprParsers.Arg_Parsed[EP.Arg_Parsed(name=:a1, type=:T1, default=ExprParsers.NoDefault())], ExprParsers.TypeRange_Parsed[EP.TypeRange_Parsed(lb=Union{}, name=:T1, ub=Any)], [:a1], [:T1]), WhereTraits.InternalState.DefOuterFuncNonFixedPart(Union{Expr, Symbol}[:(isodd(a1))], Dict{Union{Expr, Symbol}, Union{Expr, Symbol}}(:(isodd(a1)) => :((WhereTraits.Utils.BoolTypes.BoolType)(isodd(a1)))))), Dict{WhereTraits.InternalState.DefInnerFuncFixedPart, WhereTraits.InternalState.DefInnerFuncNonFixedPart}(WhereTraits.InternalState.DefInnerFuncFixedPart(Dict(:a1 => :a), Dict{Symbol, Symbol}(), Dict{Union{Expr, Symbol}, Union{Expr, Symbol}}(:(isodd(a1)) => :(::Type{<:WhereTraits.Utils.BoolTypes.True}))) => WhereTraits.InternalState.DefInnerFuncNonFixedPart(Main, Expr[], quote
        #= REPL[6]:1 =#
        (a + 1) / 2
    end, :((foo(a) where isodd(a)) = begin
        #= REPL[6]:1 =#
        (a + 1) / 2
    end))), WhereTraits.InternalState.DefDisambiguation({1, 0} directed Int64 metagraph with Float64 weights defined by :weight (default weight 1.0)))
end
nothing
```
It is actually easy to understand on a high level:

1. The first function `function foo(a1::T1; kwargs...) where T1` is the so called "outer" function which does all the normal standard Julia dispatch. It is the necessary initial entry point in order to then perform a subsequent call to further dispatch on traits.

  In the function body, within a try-catch, you see that this outer function extracts extra information according to the extended where-syntax. Lets go through the arguments one by one

  1. `WhereTraits.InternalState.TraitsDisambiguationSingleton()` is a helper type indicating that this is a call to the traits disambiguation layer
  2. `Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1` is the complete function signature of the outer function, with an additional helper `<CURLIES|SIGNATURE|ARGS>`, which is actually a `WhereTraits.Parsing.Normalize._BetweenCurliesAndArgs` to deal with TypeParameters of UnionAll types (whereupon which you can also define function calls and hence `@traits`)
  3. `a1` is the first actual argument (after this `a2`, `a3` and etc. could follow in principle)
  4. `<ARGS|TYPEVARS>()` is actually a `WhereTraits.InternalState.ArgsHelpers_BetweenArgsAndTypeVars()`, which is again a helper type to distinguish args from typevariables
  5. `T1` is a type parameter (again here `T2`, `T3`, etc. would follow if there are more typeparameters)
  6. `<TYPEVARS|TRAITS>()` is actually a `WhereTraits.InternalState.ArgsHelpers_BetweenTypeVarsAndTraits()`, which is another helper, now separating the traits definitions
  7. `(WhereTraits.Utils.BoolTypes.BoolType)(isodd(a1))` here comes our first actual trait definition (if you define more traits, they would follow here), it is a bool definition and uses the special function BoolType to bring bool values to BoolTypes.
  8. `; kwargs...` at last all kwargs are just passed through (dispatch on kwargs is not yet supported)

  All these arguments are finally passed on to the inner function, which is defined next. (For completeness, there is an intermediate disambiguation layer, which is sitting between outer function and inner function. It will be described shortly after.)

2. The second function is this inner function `function foo(::WhereTraits.InternalState.TraitsDefSingleton, ::Type{Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1}, a, ::<ARGS|TYPEVARS>, var"'T1'"::Any, ::<TYPEVARS|TRAITS>, var"'isodd(a1)'"::Type{<:WhereTraits.Utils.BoolTypes.True})`.

  Here we define the actual full traits dispatch which was just called in the outer function (we ignore the disambiguation layer for now). Let's again go through each single argument:

  1. `::WhereTraits.InternalState.TraitsDefSingleton` dispatches on the singleton type for an inner-function-definition.
  2. `::Type{Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1}` dispatches on the signature of the outer function, again adding support for types with Type-parameters which is why you see this extra type `<CURLIES|SIGNATURE|ARGS>` (which is actually a `WhereTraits.Syntax.Parsing.Normalize._BetweenCurliesAndArgs`). If you would have dispatch for say `function MyType{Int, Bool}(a::Any, b::Any)` this would look like `::Type{Tuple{Int, Bool, <CURLIES|SIGNATURE|ARGS>, T1, T2} where {T1, T2}}` respectively
  3. `a` is just the standard argument, which was of type `Any`.

    Hereafter, other arguments would follow.

  4. `::<ARGS|TYPEVARS>` (which is actually a `::WhereTraits.InternalState.ArgsHelper_BetweenArgsAndTypeVars`) is again a helper for dispatch separation
  5. `var"'T1'"::Any` corresponds to the normal standard TypeParameter, here of type Any.

    It was renamed into `var"'T1'"`, because it is actually nowhere used in the function body. If you would have used the TypeVariable `T1`, it is named plainly `T1`.
    This was implemented because the syntax actually may have to add extra typeparameters, which then for sure are not used by the code. We distinguish used/unused typeparameters for better debugging/inspecting.

    Hereafter, other standard type parameters would follow.

  6. `::<TYPEVARS|TRAITS>` (actually `::WhereTraits.InternalState.ArgsHelper_BetweenTypeVarsAndTraits`) is the last helper for dispatch separation
  7. `var"'isodd(a1)'"::Type{<:WhereTraits.Utils.BoolTypes.True}` is our extended where-dispatch for Bool function

      As the argument was wrapped into BoolTypes.BoolType, we can now dispatch on `BoolTypes.True`. The name is extra descriptive and refers to the precise function call which happens in the outer function. This can be helpful for debugging and inspecting.

  8. This function does not define any keyword arguments.

3. The third function is the disambiguation, which sits between outer function and inner function. In our case it only forwards the arguments to the inner function, but in general this layer implements the functionality of `@traits_order`. 

4. The last complex looking function is `function foo(::WhereTraits.InternalState.TraitsStoreSingleton)`. Concretely, it defines the hidden state which is needed to correctly construct the outer and inner functions required to realise the extended dispatch of `@traits`. You don't have to understand it, still you hopefully get the feeling that everything is there.

5. Finally there is `nothing` in order to prevent printing possibly confusing internal details.

If you try `@macroexpand @traits foo(a) where {!isodd(a)} = a/2` instead, you will see that it is very similar, but dispatching on `::BoolTypes.False` instead. This is part of the special support for bool function.

Also try `@macroexpand @traits foo(a) where {iseven(a)} = a/2` and see what the syntax does differently.

-----------

Now execute
```julia
@traits foo(a) where {isodd(a)} = (a+1)/2
```
and repeat inspecting
`@macroexpand @traits foo(a) where {!isodd(a)} = a/2` vs `@macroexpand @traits foo(a) where {iseven(a)} = a/2`.

Also try inspecting methods with other outerfunctions, like `@macroexpand @traits foo(a, b) = a + b`. You will start appreciating the hidden complexity behind the `@traits` syntax.

While the syntax mapping to an outerfunction and respective innerfunctions feels very intuitive, the needed implementation is surprisingly complicated. Luckily, all this is encapsulated nicely in the `@traits` macro. Enjoy!

------------

If you ever are curious what the whole implementation of your `@traits` function is, there is a helper macro `@traits_show_implementation`.
For instance if you finally defined
```julia
@traits foo(a) where {isodd(a)} = (a+1)/2
@traits foo(a) where {!isodd(a)} = a/2
```
`@traits_show_implementation` will give you the full implementation, omitting the internal state.

```juliarepl
julia> @traits_show_implementation foo
  Outer function for signature Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1

  function foo(a1::T1; kwargs...) where T1
      #= REPL[9]:1 =#
      begin
          #= /home/ssahm/.julia/dev/WhereTraits/src/Syntax/Rendering.jl:211 =#
          try
              #= /home/ssahm/.julia/dev/WhereTraits/src/Syntax/Rendering.jl:212 =#
              (Main).foo(WhereTraits.InternalState.TraitsDisambiguationSingleton(), Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1, a1, <ARGS|TYPEVARS>(), T1, <TYPEVARS|TRAITS>(), (WhereTraits.Utils.BoolTypes.BoolType)(isodd(a1)); kwargs...)
          catch exc
              #= /home/ssahm/.julia/dev/WhereTraits/src/Syntax/Rendering.jl:214 =#
              (WhereTraits.InternalState.isWhereTraitsMethodError)(exc) || rethrow()
              #= /home/ssahm/.julia/dev/WhereTraits/src/Syntax/Rendering.jl:215 =#
              throw((WhereTraitsMethodError)(exc))
          end
      end
  end)

    •  • •

  Inner functions for signature Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1

  function foo(::WhereTraits.InternalState.TraitsDefSingleton, ::Type{Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1}, a, ::<ARGS|TYPEVARS>, var"'T1'"::Any, ::<TYPEVARS|TRAITS>, var"'isodd(a1)'"::Type{<:WhereTraits.Utils.BoolTypes.True})
      #= REPL[7]:1 =#
      (a + 1) / 2
  end

    •  • •

  function foo(::WhereTraits.InternalState.TraitsDefSingleton, ::Type{Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1}, a, ::<ARGS|TYPEVARS>, var"'T1'"::Any, ::<TYPEVARS|TRAITS>, var"'isodd(a1)'"::Type{<:WhereTraits.Utils.BoolTypes.False})
      #= REPL[8]:1 =#
      a / 2
  end

    •  • •

    •  • •

  Disambiguation functions for signature Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1

  Base.delete_method(foo(::WhereTraits.InternalState.TraitsDisambiguationSingleton, ::Type{Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1}, a1, ::<ARGS|TYPEVARS>, T1, ::<TYPEVARS|TRAITS>, var"'trait_1_isodd(a1)'"; kwargs...) in Main)

    •  • •

  function foo(::WhereTraits.InternalState.TraitsDisambiguationSingleton, ::Type{Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1}, a1, ::<ARGS|TYPEVARS>, T1, ::<TYPEVARS|TRAITS>, var"'trait_1_isodd(a1)'"::Any; kwargs...)
      (Main).foo(WhereTraits.InternalState.TraitsDefSingleton(), Tuple{<CURLIES|SIGNATURE|ARGS>, T1} where T1, a1, <ARGS|TYPEVARS>(), T1, <TYPEVARS|TRAITS>(), var"'trait_1_isodd(a1)'"; kwargs...)
  end

    •  • •

  ──────────────────────────────────────────────────────────────────────────────────────
```



## Performance + Comparison with [mauro3/SimpleTraits.jl](https://github.com/mauro3/SimpleTraits.jl)

For a high-level comparison between `WhereTraits.@traits` and `SimpleTraits.jl` see the respective [discourse discussion](https://discourse.julialang.org/t/announcing-traits-jl-a-revival-of-julia-traits/35683/5).

The following examples mirror <https://github.com/mauro3/SimpleTraits.jl#details-of-method-dispatch>.
We start with defining a custom function `fn`, a custom Trait function `isTr` and some methods for `fn` dispatching on the trait.
```julia
isTr(_) = false

fn(x::Integer) = 1
@traits fn(x::X) where {X<:AbstractFloat, isTr(X)} = 2
@traits fn(x::AbstractFloat) = 3

fn(Float32(5)) # 3; dispatch through traits, but isTr not yet defined, hence using default case

isTr(::Type{Float32}) = true
isTr(::Type{Int}) = true

fn(5) # 1; dispatch only happens on the type
fn(Float32(5)) # 2; dispatch through traits
fn(Float64(5)) # 3; default dispatch through traits


isTr2(_) = false
isTr2(::Type{Float16}) = true
@traits fn(x::X) where {X <: AbstractFloat, isTr2(X)} = 4

fn(Float16(5)) # 4; dispatch through traits
fn(Float32(5)) # 2; NO MethodError; nothing is overwritten, everything works like you would hope for
```

Finally the performance test: Julia is indeed inferring the very same native code.
```julia-repl
julia> @code_native fn(5)
    .section    __TEXT,__text,regular,pure_instructions
; ┌ @ none:1 within `fn'
    movl    $1, %eax
    retq
    nopw    %cs:(%rax,%rax)
; └
julia> @code_native fn(Float16(5))
    .section    __TEXT,__text,regular,pure_instructions
; ┌ @ none:1 within `fn'
    movl    $4, %eax
    retq
    nopw    %cs:(%rax,%rax)
; └
```
