using WhereTraits
using Documenter

makedocs(;
    modules=[WhereTraits],
    authors="Stephan Sahm <stephan.sahm@gmx.de> and contributors",
    repo="https://github.com/schlichtanders/WhereTraits.jl/blob/{commit}{path}#L{line}",
    sitename="WhereTraits.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://schlichtanders.github.io/WhereTraits.jl",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
        "Manual" => [
            "`@traits`" => "manual.md",
            "WhereTraits.BasicTraits" => "basictraits.md",
            "Combination with `isdef`" => "isdef.md",
        ],
        "Library" => "library.md",
    ],
)

deploydocs(;
    repo="github.com/schlichtanders/WhereTraits.jl",
)
