-module(phone_fsm).
-behaviour(gen_fsm).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 10000).

-record(status, {phone, fsm}).

init(PhoneNumber) ->
  %hlr:attach(PhoneNumber),
  {ok, idle, #status{phone=PhoneNumber, fsm=none}}.

idle(inbound, State) ->
  {next_state, conecting, State};
idle(calling, State) ->
  {next_state, calling, State#s{fsm=Pid}, ?TIMEOUT}.

calling(connected, State) ->
  {next_state, receiving, State};
calling(hangup, State) ->
  {next_state, idle, State#s{fsm=none}}.

connecting(connected, State) ->
  {next_state, receiving, State}
connecting(rejected, State) ->
  {next_state, idle, State#s{fsm=none}}

receiving(hangup, State) ->
  {next_state, idle, State#s{fsm=none}}

handle_event(stop, _StateName, State) ->
  {stop, normal, State}.

%% PUBLIC API
-spec start_link(PhoneNumber) -> {ok, FsmPid}.
start_link(PhoneNumber) ->
  gen_fsm:start_link(?MODULE, [PhoneNumber], []).


-spec stop(FsmPid) -> ok.
stop(FsmPid) ->
  gen_fsm:send_all_state_event(FsmPid, stop)





% -spec connect(FsmPid) -> ok.
% connect(FsmPid) ->
%   gen_fsm:send_event(FsmPid, connected),
%   ok.


% -spec disconnect(FsmPid) -> ok.
% disconnect(FsmPid) ->
%   gen_fsm:send_event(FsmPid, hangup),
%   ok.


%-spec action(FsmPid, Action) -> ok.

% -spec reject(FsmPid) -> ok.

% -spec accept(FsmPid) -> ok.

% -spec hangup(FsmPid) -> ok.

% -spec inbound(FsmPid) -> ok.

% -spec inbound(FsmPid) -> ok.

