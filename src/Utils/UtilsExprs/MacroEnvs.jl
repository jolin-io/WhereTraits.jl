module MacroEnvs
export MacroEnv, @MacroEnv

@Base.kwdef struct MacroEnv
  source::LineNumberNode
  mod::Module
end

macro MacroEnv()
  esc(quote
    $MacroEnv(__source__, __module__)
  end)
end


end