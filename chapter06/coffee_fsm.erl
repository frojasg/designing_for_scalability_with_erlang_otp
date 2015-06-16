-module(coffee_fsm).
-behaviour(gen_fsm).


-export([tea/0, espresso/0, americano/0, cappuccino/0, pay/1, cup_removed/0, cancel/0]).
-export([start_link/0, init/1, start_link/2, start/2]).
-export([selection/2, payment/2, remove/2]).

start_link() ->
  gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link(TimerMs, Options) ->
  gen_fsm:start_link({local, ?MODULE}, ?MODULE, TimerMs, Options).

start(TimerMs, Options) ->
  gen_fsm:start({local, ?MODULE}, ?MODULE, TimerMs, Options).


init([]) ->
  hw:reboot(),
  hw:display("Make your selection", []),
  process_flag(trap_exit, true),
  {ok, selection, []}.

selection({selection, Type, Price}, _LoopData) ->
  hw:display("Please pay~w", [Price]),
  {next_state, payment, {Type, Price, 0}};
selection({pay, Coin}, LoopData) ->
  hw:return_change(Coin),
  {next_state, selection, LoopData};
selection(_Other, LoopData) ->
  {next_state, selection, LoopData}.

payment({pay, Coin}, {Type, Price, Paid}) when Coin+Paid < Price ->
  NewPaid = Coin + Paid,
  hw:display("Please pay:~w", [Price - NewPaid]),
  {next_state, payment, {Type, Price, NewPaid}};
payment({pay, Coin}, {Type, Price, Paid}) when Coin+Paid >= Price ->
  NewPaid = Coin + Paid,
  hw:display("Preparing Drink.",[]),
  hw:return_change(NewPaid - Price),
  hw:drop_cup(), hw:prepare(Type),
  hw:display("Remove Drink.", []),
  {next_state, remove, null};
payment(cancel, {_Type, _Price, Paid}) ->
  hw:return_change(Paid),
  hw:display("Make Your Selection", []),
  {next_state, selection, null};
payment(_Other, LoopData) ->
  {next_state, payment, LoopData}.

remove(cup_removed, LoopData) ->
  hw:display("Make Your Selection", []),
  {next_state, selection, LoopData};
remove({pay, Coin}, LoopData) ->
  hw:return_change(Coin),
  {next_state, remove, LoopData};
remove(_Other, LoopData) ->
  {next_state, remove, LoopData}.


%% Client Functions for Drink Selections
tea() -> gen_fsm:send_event(?MODULE, {selection, tea, 100}).
espresso() -> gen_fsm:send_event(?MODULE, {selection, espresso, 150}).
americano() -> gen_fsm:send_event(?MODULE, {selection, americano, 100}).
cappuccino() -> gen_fsm:send_event(?MODULE, {selection, cappuccino, 150}).

%% Client Functions for Actions
cup_removed() -> gen_fsm:send_event(?MODULE, cup_removed).
pay(Coin) -> gen_fsm:send_event(?MODULE, {pay, Coin}).
cancel() -> gen_fsm:send_event(?MODULE, cancel).


% Usage
% c(coffee_fsm).
% {ok, Pid} = coffee_fsm:start_link().
% sys:trace(Pid, true).
% coffee_fsm:cancel().
% coffee_fsm:tea().
