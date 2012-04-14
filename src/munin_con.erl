-module(munin_con).
-vsn(0.7).
-behavior(gen_fsm).

%basic api
-export([new/1, host/1, helo/1]).
% Request api
-export([version/1, list/1, cap/1, nodes/1, config/2, fetch/2]).

%for behaior
-export([init/1, terminate/3, code_change/4, handle_info/3, handle_event/3, handle_sync_event/4]).

% States
-export([greeting/2, ready/2, ready/3, recVersion/2, recLine/2, recBlock/2]).

-record(conState,{
		host=nil,
		port=4949,
		socket=nil,
		client=nil,
		field=nil,
		helo=nil,
		buffer=[]
	}).

-define(ECHO_TIMEOUT,	4500 ).
-define(DATA_TIMEOUT,	1000 ).
-define(COM_TIMEOUT,	1200 ).

% Client functions
new(Host) when is_atom(Host)->
	new({Host,4949});
new({Host})->
	new({Host,4949});
new({Host,Port})->
	gen_fsm:start_link(?MODULE,{Host,Port}, [{timeout,1000},{debug,[log,trace]}]).

%% Basic api - no request
helo(Pid) when is_pid(Pid)->
	gen_fsm:sync_send_event(Pid,getHelo).
host(Pid) when is_pid(Pid)->
	gen_fsm:sync_send_event(Pid,getHost).

% Requesting api
version(Pid) when is_pid(Pid)->
	gen_fsm:sync_send_event(Pid,getVersion).
cap(Pid) when is_pid(Pid)->
	gen_fsm:sync_send_event(Pid,getCap).
list(Pid) when is_pid(Pid)->
	gen_fsm:sync_send_event(Pid,getList).
nodes(Pid) when is_pid(Pid)->
	gen_fsm:sync_send_event(Pid,getNodes).
config(Pid,Plugin) when is_pid(Pid)->
	gen_fsm:sync_send_event(Pid,{getConfig, Plugin}).
fetch(Pid,Plugin) when is_pid(Pid)->
	gen_fsm:sync_send_event(Pid,{getFetch, Plugin}).

init({Host,Port})->
	Result=gen_tcp:connect(Host,Port,[
			{active,true},
			{packet,line},
			{nodelay,true},
			list
		]),
	case Result of
		{error, Reason} -> {stop, {"failed to connect",Reason}};
		{ok,Socket} -> {ok, greeting, #conState{host=Host,port=Port,socket=Socket}}
	end.

code_change(OldVsn, StateName, OldData, Extra) ->
	error_logger:info_msg("~p Upgrading state from vsn:~w: data: ~w, extra:~w.\n",
		[self(), OldVsn, OldData, Extra]),
	{ok, StateName, OldData}.


greeting({gotData, Helo}, State) ->
	NewState=State#conState{helo=Helo},
	{next_state, ready, NewState, ?ECHO_TIMEOUT}.

%% Async events
ready(timeout , State) ->
	ready(getVersion,nil,State).

%% Sync events
ready(getHost,_From, State) ->
	{reply, State#conState.host, ready, State, ?ECHO_TIMEOUT};
ready(getHelo,_From, State) ->
	{reply, State#conState.helo, ready, State, ?ECHO_TIMEOUT};

%% Running queries
ready(getVersion,From, State) ->
	getLinedValue(version, State, "version\n", From);
ready(getList,From, State) ->
	getLinedValue(list, State, "list\n", From);
ready(getCap,From, State) ->
	getLinedValue(cap, State, "cap\n", From);
ready(getNodes,From, State) ->
	getLinedValue(nodes, State, "nodes\n", From);
ready({getConfig,Plugin},From, State) ->
		gen_tcp:send(State#conState.socket,"config "++ Plugin ++"\n"),
		NewState=State#conState{client=From},
		{next_state, recBlock, NewState, ?ECHO_TIMEOUT};
ready({getFetch,Plugin},From, State) ->
		gen_tcp:send(State#conState.socket,"fetch "++ Plugin ++"\n"),
		NewState=State#conState{client=From},
		{next_state, recBlock, NewState, ?ECHO_TIMEOUT}.

getLinedValue(Field, State, Request, From)->
		gen_tcp:send(State#conState.socket, Request),
		NewState=State#conState{client=From,field=Field},
		{next_state, recLine, NewState, ?ECHO_TIMEOUT}.



recVersion({gotData, Version}, State) ->
	case State#conState.client of
		nil -> ok;
		Pid -> 
			gen_fsm:reply(Pid,Version)
	end,
	{next_state, ready, State#conState{client=nil}, ?ECHO_TIMEOUT}.

recLine({gotData, Line}, State) ->
	case State#conState.client of
		nil -> ok;
		Pid -> 
			gen_fsm:reply(Pid,Line)
	end,
	NewState=State#conState{field=nil,client=nil},
	{next_state, ready, NewState, ?ECHO_TIMEOUT}.

recBlock({gotData, Line}, State) ->
	case Line of
		".\n" ->
			gen_fsm:reply(State#conState.client,State#conState.buffer),
			{next_state, ready, State#conState{client=nil, buffer=[]}, ?ECHO_TIMEOUT};
		Line -> 
			NewState=State#conState{buffer=[Line|State#conState.buffer]},
			{next_state, recBlock, NewState, ?DATA_TIMEOUT}
	end.

handle_info({tcp_closed, Socket}, State, LocalState) when Socket == LocalState#conState.socket->
	gen_fsm:send_event(self(),{socket_closed}),
	NewLocalState=LocalState#conState{socket=nil},
	{next_state, State, NewLocalState};

handle_info({tcp, Socket, Data}, State, LocalState) when Socket == LocalState#conState.socket->
	gen_fsm:send_event(self(),{gotData, Data}),
	{next_state, State, LocalState}.

terminate(_Reason, _StateName, Data)->
	error_logger:info_msg("~p Terminating with data ~w.\n", [self(), Data]),
	case Data#conState.socket of
		nil->ok;
		Socket -> gen_tcp:close(Socket)
	end,
	ok.

%% Dummy functions
handle_event(Event, State, Data)->
	error_logger:error_msg("~p Got handle event - wtf? ~w ~w ~w.\n", [self(), Event, State, Data]),
	{stop,{shutdown, wtf},Data}.

handle_sync_event(Event, From, State, Data)->
	error_logger:error_msg("~p Got handle_sync_event - wtf? ~w ~w ~w ~w.\n", [self(), Event, From, State, Data]),
	{stop,{shutdown, wtf},wtf,Data}.


