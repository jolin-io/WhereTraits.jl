type = Dict{Int, String}
type_normalized = Traits.normalize_parametrictype(type)
type_parameters = Traits.parametersof(type)
key, value = iterate(dict_traitsof1_typeparams[type_normalized])[1]
for (key, value) ∈ dict_traitsof1_typeparams[type_normalized]
  println(key)
  if key isa Traits.Constrain1
    if type <: key.type && all(traitsrequire <: traitsof(p)
        for (p, traitsrequire) ∈ zip(type_parameters, key.parameter_constraints))
      println("hi")
      println(value)
    end
  else
    if type <: key
      println("hi")
      println(value)
    end
  end
  DataType()
end
