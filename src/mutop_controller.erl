-module(mutop_controller).
-author("Rafał Trójniak <rafal@trojniak.net>").
-vsn(0.1).

-behavior(gen_fsm).

-export([init/1, terminate/3, code_change/4, handle_info/3, handle_event/3, handle_sync_event/4]).

-export([start/0, stop/0, setInfo/1]).

% States
-export([running/2, running/3, discovery/2, discovery/3]).
-record(state,{
		statusBar=nil
	}).

% Global api
start() ->
	gen_fsm:start_link({local,?MODULE}, ?MODULE, [], []).
stop() ->
	gen_fsm:sync_send_event(?MODULE, stop).
setInfo(Info) when is_list(Info)->
	gen_fsm:send_all_state_event(?MODULE, {setInfo,Info}),
	ok.

cleanUp()->
	escreen_gate:unsubscribeKeys(self()),
	escreen_gate:turnOff(),
	ok.

%States
discovery({key,10},  State) ->
	{stop, shutdown, State};
discovery({key,_Key},  State) ->
	escreen_panel_status:setContent(State#state.statusBar,"got unknown command "),
	{next_state, discovery, State}.

discovery(stop, _From, State) ->
	{stop, shutdown, stopping, State}.

%States
running({key,10},  State) ->
	{stop, shutdown, State};
running({key,_Key},  State) ->
	escreen_panel_status:setContent(State#state.statusBar,"got unknown command "),
	{next_state, running, State}.

running(stop, _From, State) ->
	{stop, shutdown, stopping, State};
running(_Event, _From, State) ->
	{reply, unkown, running, State}.

% Gen_FSM api
init([]) ->
	escreen_gate:turnOn(),
	escreen_gate:subscribeKeys(self()),
	{ok, StatusBar}=escreen_panel_status:start(full, 1, 0, 0),
	escreen_panel_status:setContent(StatusBar,"controller started"),
	case application:get_env(mutop, runmode) of
		{ok, autogen} ->
			mutop_crawler:start(),
			{ok, discovery, #state{statusBar=StatusBar}};
		_ ->
			{ok, running, #state{statusBar=StatusBar}}
	end.

terminate(_Reason, _StateName, _Data)->
	cleanUp(),
	init:stop(),
	ok.

code_change(_OldVsn, StateName, OldData, _Extra) ->
	{ok, StateName, OldData}.
handle_sync_event(_Event, _From, State, Data)->
	{next_state, unknown, State, Data}.

handle_event({setInfo,Info}, State, Data)->
	escreen_panel_status:setContent(Data#state.statusBar,atom_to_list(State)++":" ++ Info),
	{next_state, State, Data}.

handle_info({key,Char}, State, LocalState) ->
	gen_fsm:send_event(self(),{key,Char}),
	{next_state,State,LocalState}.
