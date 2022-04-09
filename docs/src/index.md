# WhereTraits.jl

Welcome to `WhereTraits.jl`. This package exports one powerful macro `@traits` with which you can extend Julia's where syntax in order to support traits definitions.

In addition, `WhereTraits` comes with a standardized way how to resolve ambiguities among traits, by defining an order among the traits with `@traits_order`.

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
which brings `@traits` into your namespace, and in addition also `@traits_order` for resolving ambiguities.


## Manual Outline

```@contents
Pages = ["usage.md", "details.md", "basictraits.md"]
```

## [Library Index](@id main-index)

```@index
Pages = ["library.md"]
```
