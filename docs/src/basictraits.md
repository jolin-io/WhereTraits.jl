
# `WhereTraits.BasicTraits`

For anology with [SimpleTraits.jl](https://github.com/mauro3/SimpleTraits.jl), this package comes with standard traits definitions
`ismutable`, `isimmutable`, `isiterable`, `iscallable`, `isbitstype`, `isconcretetype`. They mostly just wrap respective standard definitions in `Base`, with the added benefit, that they behave similarly to `Base.eltype` in that they have the convenience fallback `ismutable(value) = ismutable(typeof(value))`.

You can use them by executing the following
```julia
using WhereTraits
using WhereTraits.BasicTraits
WhereTraits.BasicTraits.@overwrite_Base

ismutable(1)  # false
ismutable("string")  # true
ismutable(String)  # true
```

Please consult the file [test/BasicTraits.jl](https://github.com/jolin-io/WhereTraits.jl/blob/master/test/BasicTraits.jl) for more examples.
