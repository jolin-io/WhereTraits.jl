
# isdef, Out
# ==========

# these tests are optional, but show pretty nicely how @traits can be used
isdef_defined = try
  using IsDef
  true
catch e
  false
end

if isdef_defined
  struct MyError <: Exception
    msg::AbstractString
  end
  MyError() = MyError("")

  @traits_test typesafe_call(f, a::T) where {T, isdef(f, T)} = f(a)
  # not that you can also use isdef on args directly, like you can use eltype on args
  # however this will be recognized as a different traitfunction compared to isdef(f, T)
  # which can lead to ambiguous overloadings. Just good to keep in mind
  @traits_test typesafe_call(f, a) where {!isdef(f, a)} = throw(MyError("given function cannot work with given arg"))
  @code_llvm typesafe_call(x -> x+2, 4)  # looks really good

  @test typesafe_call(x -> x+2, 4) == 6
  @test_throws MyError typesafe_call(x -> x+2, "string")
  @traits_show_implementation typesafe_call


  @traits_test typesafe_out(f, a) where {Out(f, a) <: Number} = f(a) + 6
  @traits_test typesafe_out(f, a) where {Out(f, a) <: String} = "yeah $(f(a))!"
  @traits_test typesafe_out(f, a) = throw(MyError("no match"))

  @test typesafe_out(x -> 3x, 1) == 9
  @test typesafe_out(x -> "$x $x", 1) == "yeah 1 1!"
  @test_throws MyError typesafe_out(x -> convert(Vector, x), 1)


  # more complex case with dependencies among functions
  @traits_test function typesafe_aggregate(a::Vector{A}, introduce, combine) where
      {A, isdef(introduce, A), isdef(combine, Out(introduce, A), Out(introduce, A))}

    reduce(combine, introduce.(a))
  end
  @traits_test typesafe_aggregate(a::Vector, introduce, combine) = throw(MyError("TypeError"))

  @test typesafe_aggregate(["this","is","a","test"], length, +) == 11
  @test typesafe_aggregate(["this","is","a","test"], length, *) == 32
  @test typesafe_aggregate(["this","is","a","test"], x -> "$x ", *) == "this is a test "
  @test_throws MyError typesafe_aggregate(["this","is","a", "test"], x->x, +)

end
