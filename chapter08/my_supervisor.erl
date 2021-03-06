-module(my_supervisor).
-export([start/2, init/1, stop/1]).

start(Name, ChildSpecList) ->
  register(Name, Pid = spawn(?MODULE, init, [ChildSpecList])),
  {ok, Pid}.

stop(Name) -> Name ! stop.

init(ChildSpecList) ->
  process_flag(trap_exit, true),
  loop(start_children(ChildSpecList)).

start_children(ChildSpecList) ->
  [{element(2, apply(M, F, A)), {M, F, A}} || {M, F, A} <- ChildSpecList].

loop(ChildList) ->
  receive
    {'EXIT', Pid, normal} ->
      loop(lists:keydelete(Pid,1,ChildList));
    {'EXIT', Pid, _Reason} ->
      NewChildList = restart_child(Pid, ChildList),
      loop(NewChildList);
    stop ->
      terminate(ChildList)
  end.

restart_child(Pid, ChildList) ->
  {value, {Pid, {M,F,A}}} = lists:keysearch(Pid, 1, ChildList),
  {ok, NewPid} = apply(M,F,A),
  [{NewPid, {M,F,A}}|lists:keydelete(Pid,1,ChildList)].

terminate(ChildList) ->
  lists:foreach(fun({Pid, _}) -> exit(Pid, kill) end, ChildList).



