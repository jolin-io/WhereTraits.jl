module UtilsGenerics
export nonempty, issomething, @iftrue, next!, filtersplit

const nonempty = !isempty
const issomething = !isnothing

next!(iter::Base.Iterators.Stateful) = iterate(iter)[1]

macro iftrue(expr)
  expr = esc(expr)
  quote
    $expr || return false
  end
end

filtersplit(f, a) = filter(f, a), filter(!f, a)

end  # module