-module(escreen_gate).
-include("encurses.hrl").
-author("Rafał Trójniak <rafal@trojniak.net>").
-vsn(0.1).

-behavior(gen_fsm).

-export([init/1, terminate/3, code_change/4, handle_info/3, handle_event/3, handle_sync_event/4]).

-export([start/0, stop/0, turnOn/0, turnOff/0, subscribeKeys/1, unsubscribeKeys/1 ]).

% States
-export([off/3, on/3]).
-record(state,{
		keySubs=[],
		keyFetcher=nil
	}).

% Global api
start() ->
	gen_fsm:start_link({local,?MODULE}, ?MODULE, [], []).
stop() ->
	gen_fsm:sync_send_event(?MODULE, stop).
turnOn()->
	gen_fsm:sync_send_event(?MODULE, turnOn).
turnOff()->
	gen_fsm:sync_send_event(?MODULE, turnOff).
subscribeKeys(Pid) when is_pid(Pid)->
	gen_fsm:sync_send_all_state_event(?MODULE, {subscribe,Pid}).
unsubscribeKeys(Pid) when is_pid(Pid)->
	gen_fsm:sync_send_all_state_event(?MODULE, {unsubscribe,Pid}).

cleanup()->
	encurses:erase(),
	encurses:refresh(),
	encurses:endwin(),
	encurses:echo(),
	ok.

%States
on(turnOff, _From, State) ->
	cleanup(),
	case State#state.keyFetcher  of
		nil ->
			{reply, ok, off, State};
		Pid when is_pid(Pid) ->
			Pid ! stop,
			{reply, ok, off, State#state{keyFetcher=nil} }
	end;

on(_Event, _From, State) ->
	{reply, unkown, on, State}.

off(turnOn, _From, State) ->
	encurses:initscr(),
	encurses:keypad(0, true),
	encurses:curs_set(?CURS_INVISIBLE),
	encurses:erase(),
	encurses:noecho(),
	encurses:refresh(),
	MyPid=self(),
	Fetcher=spawn(fun()-> keyFetcher(MyPid) end ),
	{reply, ok, on, State#state{keyFetcher=Fetcher}};
off(_Event, _From, State) ->
	{reply, unkown, off, State}.

% Gen_FSM api
init([]) ->
	{ok, off,#state{}}.

terminate(_Reason, _StateName, _Data)->
	ok.

code_change(OldVsn, StateName, OldData, Extra) ->
	error_logger:info_msg("~p Upgrading state from vsn:~w: data: ~w, extra:~w.\n",
		[self(), OldVsn, OldData, Extra]),
	{ok, StateName, OldData}.

handle_sync_event({subscribe,Pid}, _From, State, Data)->
	{reply, ok, State ,Data#state{keySubs=[Pid|Data#state.keySubs]}};
handle_sync_event({unsubscribe,Pid}, _From, State, Data)->
	{reply, ok, State ,Data#state{keySubs=lists:delete(Pid,Data#state.keySubs)}}.

handle_event(Event, State, Data)->
	error_logger:error_msg("~p Got handle event - wtf? ~w ~w ~w.\n", [self(), Event, State, Data]),
	{stop,{shutdown, wtf},Data}.

handle_info({key, Char}, State, LocalState) ->
	if
		length(LocalState#state.keySubs)>0 ->
			[Sub|_]=LocalState#state.keySubs,
			Sub ! {key, Char};
		true->true
	end,
	{next_state,State,LocalState}.

keyFetcher(MasterPid)->
    Char=encurses:getch(),
		MasterPid ! {key,Char},
		keyFetcher(MasterPid).
