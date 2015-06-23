-module(counters).
-behaviour(gen_event).
-export([init/1, terminate/2, handle_event/2, handle_info/2]).
-export([get_counters/1, handle_call/2]).

get_counters(Pid) ->
  gen_event:call(Pid, counters, get_counters).

init(_)  ->
  TableId = ets:new(counters, []),
  {ok, TableId}.

terminate(_Reason, TableId) ->
  Counters = ets:tab2list(TableId),
  ets:delete(TableId),
  {counters, Counters}.

handle_event(Event, TableId) ->
  try ets:update_counter(TableId, Event, 1) of
  _ok -> {ok, TableId}
  catch
  error:_ -> ets:insert(TableId, {Event, 1}),
           {ok, TableId}
  end.

handle_call(get_counters, TableId) ->
  {ok, {counters, ets:tab2list(TableId)}, TableId}.

handle_info(_, TableId) ->
  {ok, TableId}.

% {ok, P} = gen_event:start().
%  gen_event:add_handler(P, counters, {}).
%  gen_event:notify(P, {set_alarm, {no_frequency, self()}}).
%  gen_event:notify(P, {event, {frequency_denied, self()}}).
%  gen_event:notify(P, {event, {frequency_denied, self()}}).
%  counters:get_counters(P).
%
