# adapted from https://raw.githubusercontent.com/rawrgrr/Levenshtein.jl
"""
Simple module for measuring distance between 2 strings
"""
module Levenshtein
export levenshtein

function levenshtein(source::AbstractString, target::AbstractString;
    deletion_cost::Number = 1, insertion_cost::Number = 1, substitution_cost::Number = 1)

  if length(target) == 0
    return length(source) * deletion_cost
  elseif length(source) < length(target)
    # Space complexity of function = O(length(target))
    return levenshtein(target, source; insertion_cost = insertion_cost, deletion_cost = deletion_cost, substitution_cost = substitution_cost)
  end

  cost_type = promote_type(typeof(deletion_cost), typeof(insertion_cost), typeof(substitution_cost))
  costs::Matrix = Array{cost_type}(undef, 2, length(target) + 1)

  old_cost_index = 1
  new_cost_index = 2

  costs[old_cost_index, 1] = 0
  for i in 1:length(target)
    costs[old_cost_index, i + 1] = i * insertion_cost
  end

  i = 0
  for r in source
    i += 1
    # Delete i characters from source to get empty target
    costs[new_cost_index, 1] = i * deletion_cost

    j = 0
    for c in target
      j += 1

      deletion = costs[old_cost_index, j + 1] + deletion_cost
      insertion = costs[new_cost_index, j] + insertion_cost
      substitution = costs[old_cost_index, j]
      if r != c
        substitution += substitution_cost
      end
      costs[new_cost_index, j + 1] = min(deletion, insertion, substitution)
    end
    old_cost_index, new_cost_index = new_cost_index, old_cost_index
  end
  new_cost_index = old_cost_index
  costs[new_cost_index, length(target) + 1]
end

end # module
