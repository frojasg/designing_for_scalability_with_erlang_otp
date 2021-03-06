
-module(logger).
-behaviour(gen_event).
-export([init/1, terminate/2, handle_event/2, handle_info/2]).

init(standard_io)  ->
  {ok, {standard_io, 1}};
init({file, File}) ->
  {ok, Fd} = file:open(File, write),
  {ok, {Fd, 1}};
init(Args) ->
  {error, {args, Args}}.

terminate(_Reason, {standard_io, Count}) ->
  {count, Count};
terminate(_Reason, {Fd, Count}) ->
  file:close(Fd),
  {count, Count}.

handle_event(Event, {Fd, Count}) ->
  print(Fd, Count, Event, "Event"),
  {ok, {Fd, Count+1}}.

print(Fd, Count, Event, Tag) ->
  io:format(Fd,"Id:~w Time:~w Date:~w~n"++Tag++":~w~n",[Count,time(),date(),Event]).

handle_info(Event, {Fd, Count}) ->
  print(Fd, Count, Event, "Unknown"),
  {ok, {Fd, Count+1}}.


%Usage
% {ok, P} = gen_event:start()
% gen_event:add_handler(P, logger, {file, "alarmlog"}).
%  gen_event:notify(P, {set_alarm, {no_frequency, self()}}).
%  gen_event:sync_notify(P, {clear_alarm, no_frequency}).
%  gen_event:add_handler(P, logger, standard_io).
%  gen_event:notify(P, {set_alarm, {no_frequency, self()}}).
%  P ! sending_junk.
%  {ok, Binary} = file:read_file("alarmlog").
%  io:format(Binary).
%   gen_event:delete_handler(P, freq_overload, stop).
%
