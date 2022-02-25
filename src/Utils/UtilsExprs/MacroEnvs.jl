module MacroEnvs
export MacroEnv, @MacroEnv

@Base.kwdef struct MacroEnv
  source::LineNumberNode
  mod::Module
end

macro MacroEnv()
  quote
    MacroEnv($(esc(:__source__)), $(esc(:__module__)))
  end
end

end