# reported at https://discourse.julialang.org/t/unreachable-reached-with-dict-ntuple-union/19328

abstract type Tag end

function normalize_parametrictype(::Type{T}) where T
  TInternal = Base.unwrap_unionall(T).name
  # both following versions to access normalized parametric type lead to unreachable reached:
  getfield(TInternal.module, TInternal.name)
  # Core.eval(TInternal.module, TInternal.name)
end

struct MyDict
  typeparams::Dict{NTuple{N, Type}, Dict{Union{NTuple{N, Type}, Tag}, Type}} where N  # unreachable reached
  # typeparams::Dict{Tuple{Type}, Dict{Union{Tuple{Type}, Tag}, Type}}  # no NTuple | all fine
  # typeparams::Dict{NTuple{N, Type}, Dict{Union{NTuple{N, Type}}, Type}} where N  # no Tag | all fine
  function MyDict()
    typeparams = Dict{NTuple{1, Type}, Dict{Union{NTuple{1, Type}, Tag}, Type}}()  # unreachable reached
    # typeparams = Dict{Tuple{Type}, Dict{Union{Tuple{Type}, Tag}, Type}}()  # no NTuple | all fine
    # typeparams = Dict{NTuple{1, Type}, Dict{Union{NTuple{1, Type}}, Type}}()  # no Tag | all fine
    new(typeparams)
  end
end
const mydict = MyDict()

function Base.setindex!(mydict::MyDict, ::Type{Value}, types::Vararg{Type, N}) where Value where N
  types_normalized = normalize_parametrictype.(types)  # unreachable reached
  # types_normalized = types  # all fine !!!!!!!
  println("same $(types .=== types_normalized)")  # shows true in both cases!!!!!!!!!!!!
  # create empty subdict if not existing
  subdict = get!(mydict.typeparams, types_normalized) do
    Dict{Union{NTuple{N, Type}, Tag}, Type}()  # unreachable reached
    # Dict{Union{Tuple{Type}, Tag}, Type}()  # no NTuple | all fine
    # Dict{Union{NTuple{N, Type}}, Type}()  # no Tag | all fine
  end
  subdict[types] = Value
end


struct MySingleton end
mydict[Dict] = MySingleton

#=
same (true,)

Unreachable reached at 000000000B6F013D

Please submit a bug report with steps to reproduce this fault, and any error messages that follow (i
n their entirety). Thanks.
Exception: EXCEPTION_ILLEGAL_INSTRUCTION at 0xb6f013d -- setindex! at .\none:21
in expression starting at no file:0
setindex! at .\none:21
jl_fptr_trampoline at /home/Administrator/buildbot/worker/package_win64/build/src\gf.c:1831
jl_apply_generic at /home/Administrator/buildbot/worker/package_win64/build/src\gf.c:2184
do_call at /home/Administrator/buildbot/worker/package_win64/build/src\interpreter.c:324
eval_value at /home/Administrator/buildbot/worker/package_win64/build/src\interpreter.c:430
eval_stmt_value at /home/Administrator/buildbot/worker/package_win64/build/src\interpreter.c:363 [in
lined]
eval_body at /home/Administrator/buildbot/worker/package_win64/build/src\interpreter.c:678
jl_interpret_toplevel_thunk_callback at /home/Administrator/buildbot/worker/package_win64/build/src\
interpreter.c:795
unknown function (ip: FFFFFFFFFFFFFFFE)
unknown function (ip: 00000000057F36BF)
unknown function (ip: FFFFFFFFFFFFFFFF)
jl_toplevel_eval_flex at /home/Administrator/buildbot/worker/package_win64/build/src\toplevel.c:813
jl_toplevel_eval_in at /home/Administrator/buildbot/worker/package_win64/build/src\builtins.c:622
eval at .\boot.jl:319 [inlined]
repleval at C:\Users\MYUSERNAME\.julia\packages\Atom\7rQ1O\src\repl.jl:139
jl_apply_generic at /home/Administrator/buildbot/worker/package_win64/build/src\gf.c:2184
#164 at C:\Users\MYUSERNAME\.julia\packages\Atom\7rQ1O\src\repl.jl:161
with_logstate at .\logging.jl:397
with_logger at .\logging.jl:493 [inlined]
evalrepl at C:\Users\MYUSERNAME\.julia\packages\Atom\7rQ1O\src\repl.jl:152
jl_apply_generic at /home/Administrator/buildbot/worker/package_win64/build/src\gf.c:2184
do_call at /home/Administrator/buildbot/worker/package_win64/build/src\interpreter.c:324
eval_value at /home/Administrator/buildbot/worker/package_win64/build/src\interpreter.c:430
eval_stmt_value at /home/Administrator/buildbot/worker/package_win64/build/src\interpreter.c:363 [in
lined]
eval_body at /home/Administrator/buildbot/worker/package_win64/build/src\interpreter.c:678
jl_interpret_toplevel_thunk_callback at /home/Administrator/buildbot/worker/package_win64/build/src\
interpreter.c:795
unknown function (ip: FFFFFFFFFFFFFFFE)
unknown function (ip: 00000000057F348F)
unknown function (ip: FFFFFFFFFFFFFFFF)
jl_toplevel_eval_flex at /home/Administrator/buildbot/worker/package_win64/build/src\toplevel.c:813
jl_toplevel_eval_in at /home/Administrator/buildbot/worker/package_win64/build/src\builtins.c:622

Julia has exited. Press Enter to start a new session.
eval at .\boot.jl:319
jl_apply_generic at /home/Administrator/buildbot/worker/package_win64/build/src\gf.c:2184
eval_user_input at C:\cygwin\home\Administrator\buildbot\worker\package_win64\build\usr\share\julia\
stdlib\v1.0\REPL\src\REPL.jl:85
macro expansion at C:\cygwin\home\Administrator\buildbot\worker\package_win64\build\usr\share\julia\
stdlib\v1.0\REPL\src\REPL.jl:117 [inlined]
#28 at .\task.jl:259
jl_apply_generic at /home/Administrator/buildbot/worker/package_win64/build/src\gf.c:2184
jl_apply at /home/Administrator/buildbot/worker/package_win64/build/src\julia.h:1537 [inlined]
start_task at /home/Administrator/buildbot/worker/package_win64/build/src\task.c:268
Allocations: 21491315 (Pool: 21486625; Big: 4690); GC: 48
=#
