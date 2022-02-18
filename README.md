<h1> WhereTraits.jl </h1>

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://schlichtanders.github.io/WhereTraits.jl/stable)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://schlichtanders.github.io/WhereTraits.jl/dev)
[![Build Status](https://github.com/schlichtanders/WhereTraits.jl/workflows/CI/badge.svg)](https://github.com/schlichtanders/WhereTraits.jl/actions)
[![Coverage](https://codecov.io/gh/schlichtanders/WhereTraits.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/schlichtanders/WhereTraits.jl)


Welcome to `WhereTraits.jl`. This package exports one powerful macro `@traits` with which you can extend Julia's
where syntax. Concretely the following are supported:
- dispatch on functions returning Bool
```julia
@traits f(a) where {isodd(a)} = (a+1)/2
@traits f(a) where {!isodd(a)} = a/2
f(4) # 2.0
f(5) # 3.0
```
- dispatch on functions returning anything
```julia
@traits g(a) where {Base.IteratorSize(a)::Base.HasShape} = 43
@traits g(a) = 1
g([1,2,3]) # 43
g(Iterators.repeated(1)) # 1
```
- dispatch on upperbounds on functions returning Types
```julia
@traits h(a) where {eltype(a) <: Number} = true
@traits h(a) = false
h([1.0]) # true
h([""]) # false
```

And all this works with arbitrary many where expressions and creates optimal code where possible via standard Julia compiler.

For more details, take a look at the [documentation](https://schlichtanders.github.io/WhereTraits.jl/dev).

*Warning: While the dispatch works for dynamic functions, it will only be able to create optimal code if your traits function supports proper type-inference. E.g. you can use `Base.isempty`, however type-inference cannot see whether it will return true or false by static inspection. Hence it will use slower dynamic code.*


## Installation & Import

Install by running
```julia
using Pkg
pkg"add WhereTraits"
```

Then use this package by loading
```julia
using WhereTraits
```
which makes one macro available `@traits`
