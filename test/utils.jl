using Test
using Traits
using Traits.Utils
d = TypeDict(Tuple{Array} => 5, Tuple{Array} => 8)

d[Tuple{Array}] = 98
@test d[Tuple{Array}] == 98
d[Tuple{Array{T}} where T] = 3
@test d[Tuple{Array}] == 3
