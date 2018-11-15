
use this package by loading
```julia
using Traits
```



Performance + Comparison with [mauro3/SimpleTraits.jl](https://github.com/mauro3/SimpleTraits.jl)
-----------------------------------------------------------

The following examples mirrors  https://github.com/mauro3/SimpleTraits.jl#performance.
We start with defining a custom function ``fn``, a custom Trait ``Tr`` and some methods for ``fn`` dispatching on the trait.
As you see, this package recommends to use julia's standard dispatch mechanism instead of custom macro definitions. This way the interactions with normal, non-trait-based dispatch becomes way more natural.
```julia
fn(x::Integer) = 1

struct Tr end
fn(x::X) where X = fn(x, traitsof(X))
fn(x::AbstractFloat, ::TypeLB(Tr)) = 2
fn(x::AbstractFloat, _) = 3

traitsof[Float32] = Tr
traitsof[Int] = Tr

fn(5) # -> 1; dispatch only happens on the type
fn(Float32(5)) # -> 2; dispatch through traits
fn(Float64(5)) # -> 3; default dispatch through traits
```
Dispatching on a different Trait does not interfere like in mauro3/SimpleTraits.jl, however works like you would expect it from julia's method dispatch.
```julia
struct Tr2 end
traitsof[Float16] = Tr2
fn(x::AbstractFloat, ::TypeLB(Tr2)) = 4

fn(Float16(5)) # -> 4; dispatch through traits
fn(Float32(5)) # -> 2; NO MethodError; nothing is overwritten, everything works like you would hope for
```
Finally the performance test: Julia is indeed inferring the very same native code.
```julia-repl
julia> @code_native fn(5)
        .text
; Function fn {
; Location: tmp.jl:1
        pushq   %rbp
        movq    %rsp, %rbp
        movl    $1, %eax
        popq    %rbp
        retq
        nopl    (%rax,%rax)
;}
julia> @code_native fn(Float16(5))
        .text
; Function fn {
; Location: tmp.jl:4
        pushq   %rbp
        movq    %rsp, %rbp
        movl    $4, %eax
        popq    %rbp
        retq
        nopl    (%rax,%rax)
;}
```
