using WhereTraits
using Documenter

# The DocTestSetup does not work for our case, quite unsatisfying
# DocMeta.setdocmeta!(WhereTraits, :DocTestSetup, quote
#     using WhereTraits
#     using WhereTraits.BasicTraits
#     WhereTraits.BasicTraits.@overwrite_Base
# end, recursive=true)

makedocs(;
    modules=[WhereTraits, WhereTraits.BasicTraits],
    authors="Stephan Sahm <stephan.sahm@gmx.de> and contributors",
    repo="https://github.com/jolin-io/WhereTraits.jl/blob/{commit}{path}#L{line}",
    sitename="WhereTraits.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://jolin-io.github.io/WhereTraits.jl",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
        "Manual" => [
            "WhereTraits Usage`" => "usage.md",
            "WhereTraits Details`" => "details.md",
            "WhereTraits.BasicTraits" => "basictraits.md",
            # "Combination with `isdef`" => "isdef.md",
        ],
        "Library" => "library.md",
    ],
    doctest = true,  # we haven't been able to get doctest running
)

deploydocs(;
    repo="github.com/jolin-io/WhereTraits.jl",
    devbranch="main",
)
