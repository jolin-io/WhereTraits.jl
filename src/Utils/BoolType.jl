abstract type BoolType end
struct True <: BoolType end
struct False <: BoolType end

BoolType(bool::Bool) = bool ? True : False

import Base: !
!(::Type{True}) = False
!(::Type{False}) = True