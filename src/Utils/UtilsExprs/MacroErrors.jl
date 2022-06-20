module MacroErrors
export MacroError

struct MacroError{E}
    exception::E
end

function Base.showerror(io::IO, exc::MacroError)
    print(io, """
    THIS SHOULD NOT BE SEEN.
    MacroError should not be thrown directly,
    but unpacked and the inner error should be
    thrown as a result of the macro call.""")
end
  
end