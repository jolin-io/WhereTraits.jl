Manual
======

TBD


## Installation

The package is soon going to be registered at General, until then you can use it by adding a custom registry.
```julia
using Pkg
pkg"registry add https://github.com/JuliaRegistries/General"  # central julia registry
pkg"registry add https://github.com/schlichtanders/SchlichtandersJuliaRegistry.jl"  # custom registry
pkg"add WhereTraits"
```

Use it like
```julia
using WhereTraits
```
