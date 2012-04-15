-module(munin_con).
-author("Rafał Trójniak <rafal@trojniak.net>").
-vsn(1.0).
-behavior(gen_fsm).

%basic api
-export([new/1, destroy/1, host/1, helo/1]).
% Request api
-export([version/1, list/1, cap/1, nodes/1, config/2, fetch/2]).

%for behaior
-export([init/1, terminate/3, code_change/4, handle_info/3, handle_event/3, handle_sync_event/4]).

% States
-export([ ready/2, ready/3, recLine/2, recLine/3, recBlock/2, recBlock/3]).

-record(conState,{
		host=nil,
		port=4949,
		socket=nil,
		client=nil,
		field=nil,
		helo=nil,
		buffer=[]
	}).

-define(HELO_TIMEOUT,	1000 ).
-define(ECHO_TIMEOUT,	4500 ).
-define(DATA_TIMEOUT,	11000 ).
-define(COM_TIMEOUT,	11100 ).

% Client functions
new(Host) when is_atom(Host)->
	new({Host,4949});
new({Host})->
	new({Host,4949});
new({Host,Port})->
	gen_fsm:start_link(?MODULE,{Host,Port}, [{timeout,1000}]).

destroy(Pid) when is_pid(Pid)->
	gen_fsm:sync_send_event(Pid,stop).

%% Basic api - no request
%% TODO remove,as it should be implemented in all states
helo(Pid) when is_pid(Pid)->
	gen_fsm:sync_send_event(Pid,getHelo).
host(Pid) when is_pid(Pid)->
	gen_fsm:sync_send_event(Pid,getHost).

% Requesting api
version(Pid) when is_pid(Pid)->
	gen_fsm:sync_send_event(Pid,getVersion,?COM_TIMEOUT).
cap(Pid) when is_pid(Pid)->
	case gen_fsm:sync_send_event(Pid,getCap,?COM_TIMEOUT) of
		{ok, Line } -> {ok, string:tokens(Line," ")};
		Other -> Other
	end.
list(Pid) when is_pid(Pid)->
	gen_fsm:sync_send_event(Pid,getList,?COM_TIMEOUT).
nodes(Pid) when is_pid(Pid)->
	gen_fsm:sync_send_event(Pid,getNodes,?COM_TIMEOUT).
config(Pid,Plugin) when is_pid(Pid)->
	gen_fsm:sync_send_event(Pid,{getConfig, Plugin},?COM_TIMEOUT).
fetch(Pid,Plugin) when is_pid(Pid)->
	gen_fsm:sync_send_event(Pid,{getFetch, Plugin},?COM_TIMEOUT).

init({Host,Port})->
	Result=gen_tcp:connect(Host,Port,[
			{active,true},
			{packet,line},
			{nodelay,true},
			list
		]),
	case Result of
		{error, Reason} -> {stop, {"failed to connect",Reason}};
		{ok,Socket} ->
			receive
				{tcp, Socket, Helo}->
					StripedHelo=string:strip(Helo,right,10),
					{ok,
						ready,
						#conState{host=Host,port=Port,socket=Socket,helo=StripedHelo},
						?ECHO_TIMEOUT}
				after ?HELO_TIMEOUT ->
						gen_tcp:close(Socket),
						{stop, greetingTimeout}
				end
	end.

code_change(OldVsn, StateName, OldData, Extra) ->
	error_logger:info_msg("~p Upgrading state from vsn:~w: data: ~w, extra:~w.\n",
		[self(), OldVsn, OldData, Extra]),
	{ok, StateName, OldData}.



%% Async events
ready(timeout , State) ->
	ready(getVersion,nil,State);
ready(socket_closed, State) ->
	{stop,normal,State}.


%% Sync events
ready(getHost,_From, State) ->
	{reply, {ok,State#conState.host}, ready, State, ?ECHO_TIMEOUT};
ready(getHelo,_From, State) ->
	{reply, {ok,State#conState.helo}, ready, State, ?ECHO_TIMEOUT};

ready(stop,_From, State) ->
	{stop, normal, ok, State};

%% Running queries
ready(getVersion,From, State) ->
	getLinedValue(version, State, "version\n", From);
ready(getList,From, State) ->
	getLinedValue(list, State, "list\n", From);
ready(getCap,From, State) ->
	getLinedValue(cap, State, "cap\n", From);
ready(getNodes,From, State) ->
		gen_tcp:send(State#conState.socket,"nodes\n"),
		{next_state, recBlock, State#conState{client=From}, ?DATA_TIMEOUT};
ready({getConfig,Plugin},From, State) ->
		gen_tcp:send(State#conState.socket,"config "++ Plugin ++"\n"),
		{next_state, recBlock, State#conState{client=From}, ?DATA_TIMEOUT};
ready({getFetch,Plugin},From, State) ->
		gen_tcp:send(State#conState.socket,"fetch "++ Plugin ++"\n"),
		{next_state, recBlock, State#conState{client=From}, ?DATA_TIMEOUT}.

getLinedValue(Field, State, Request, From)->
		gen_tcp:send(State#conState.socket, Request),
		NewState=State#conState{client=From,field=Field},
		{next_state, recLine, NewState, ?DATA_TIMEOUT}.

recLine(stop,_From, State) ->
	case State#conState.client of
		nil -> ok;
		Pid ->
			gen_fsm:reply(Pid,{error,closing})
	end,
	NewState=State#conState{field=nil,client=nil},
	{stop, normal, ok, NewState}.

recLine({gotData, Line}, State) ->
	case State#conState.client of
		nil -> ok;
		Pid ->
			gen_fsm:reply(Pid,{ok,string:strip(Line,right,10)})
	end,
	NewState=State#conState{field=nil,client=nil},
	{next_state, ready, NewState, ?ECHO_TIMEOUT}.

recBlock(stop,_From, State) ->
	gen_fsm:reply(State#conState.client,{error,closing}),
	{stop, normal, ok, State#conState{client=nil, buffer=[]}}.

recBlock({gotData, Line}, State) ->
	case Line of
		".\n" ->
			gen_fsm:reply(State#conState.client,{ok,State#conState.buffer}),
			{next_state, ready, State#conState{client=nil, buffer=[]}, ?ECHO_TIMEOUT};
		Line ->
			Striped=string:strip(Line,right,10),
			NewState=State#conState{buffer=[Striped|State#conState.buffer]},
			{next_state, recBlock, NewState, ?DATA_TIMEOUT}
	end;

recBlock(timeout, State) ->
		gen_fsm:reply(
			State#conState.client,
			{error,{timeoutDuringReply,State#conState.buffer}}),
		NewState=State#conState{client=nil,buffer=[]},
		{stop,timeoutDuringBlockRead,NewState}.

handle_info({tcp_closed, Socket}, State, LocalState) when Socket == LocalState#conState.socket->
	gen_fsm:send_event(self(),socket_closed),
	NewLocalState=LocalState#conState{socket=nil},
	{next_state, State, NewLocalState};

handle_info({tcp, Socket, Data}, State, LocalState) when Socket == LocalState#conState.socket->
	gen_fsm:send_event(self(),{gotData, Data}),
	{next_state, State, LocalState}.

terminate(_Reason, _StateName, Data)->
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


