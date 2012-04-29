-module(munin_client_pool).
-author("Rafał Trójniak <rafal@trojniak.net>").
-vsn(0.1).
-behavior(gen_server).
% This is connection pool manager for single destination host
% It also:
% - creates connections to hosts and keeps track of them
% - monitors connection pool
% - blance requests to the connections
% TODO - Restarting dead connections

%gen_server requirements
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state,{
		proxies=[],
		host=nil
	}).
-record(proxyInfo,{
		proxy=nil,
		queue=0
	}).

-define(WAITING_QUEUE,2).
-define(PROXY_LIMIT,3).
-define(QUERY_TIMEOUT,30100).

%general api
-export([start/1, stop/1]).

%request api
-export([list/1, config/2, fetch/2, version/1, cap/1, nodes/1]).

%API for proxy
-export([requestDone/2]).

% These are all wrappers for calls to the server
start(Host) -> gen_server:start(?MODULE, Host, []).
stop(Pid) when is_pid(Pid) -> gen_server:call(Pid, stop).

% Restuest api
list(Pid) when is_pid(Pid) ->
	gen_server:call(Pid,list,?QUERY_TIMEOUT).
cap(Pid) when is_pid(Pid) ->
	gen_server:call(Pid,cap,?QUERY_TIMEOUT).
nodes(Pid) when is_pid(Pid) ->
	gen_server:call(Pid,nodes,?QUERY_TIMEOUT).
version(Pid) when is_pid(Pid) ->
	gen_server:call(Pid,version,?QUERY_TIMEOUT).
config(Pid, Plugin) when is_pid(Pid) and is_list(Plugin) ->
	gen_server:call(Pid,{config,Plugin},?QUERY_TIMEOUT).
fetch(Pid, Plugin) when is_pid(Pid) and is_list(Plugin) ->
	gen_server:call(Pid,{fetch,Plugin},?QUERY_TIMEOUT).

% Proxy api
requestDone(MyPid,ProxyPid) when is_pid(MyPid) and is_pid(ProxyPid) ->
	gen_server:cast(MyPid,{decQueue,ProxyPid}).

init(Host) ->
	{ok, #state{host=Host}}.

handle_call(stop, _From, State) ->
	{stop, normal, ok, State};
handle_call(Query, From, State) ->
	{ProxyInfo,Count} = findProxy(State),
	if
		ProxyInfo == nil ;
		Count < ?PROXY_LIMIT andalso ProxyInfo#proxyInfo.queue >= ?WAITING_QUEUE
		->
			case addProxy(State) of 
				{ok,BigerState,NewProxy} ->
					IncreasedState=incQueue(BigerState,NewProxy),
					munin_client_proxy:call(NewProxy#proxyInfo.proxy,From,Query),
					{noreply, IncreasedState};
				{error,Reason} ->
					{reply,{error,Reason},State}
			end;
		true ->
			IncreasedState=incQueue(State,ProxyInfo),
			munin_client_proxy:call(ProxyInfo#proxyInfo.proxy,From,Query),
			{noreply, IncreasedState}
	end.

% Increases queue length for ProxyInfo (Info) , returns new state
incQueue(State,Info) ->
	IncInfo=Info#proxyInfo{queue=Info#proxyInfo.queue+1},
	OldList=State#state.proxies,
	NewList=lists:keyreplace(IncInfo#proxyInfo.proxy,
		#proxyInfo.proxy,
		OldList,
		IncInfo),
	State#state{proxies=NewList}.

% Decreases queue length for ProxyPid proxy, returns new state
decQueue(State,ProxyPid) ->
	OldList=State#state.proxies,
	{value,OldInfo,ShorterList}=lists:keytake(ProxyPid,#proxyInfo.proxy,OldList),
	NewInfo=OldInfo#proxyInfo{queue=OldInfo#proxyInfo.queue-1},
	NewList=[NewInfo|ShorterList],
	State#state{proxies=NewList}.

% Adds new proxy process to the sate
addProxy(State) ->
	case munin_client_proxy:start(State#state.host,self()) of
		{error, Reason} ->
			{error, Reason};
		{ok,Proxy} ->
			Info=#proxyInfo{proxy=Proxy,queue=0},
			NewProxies=[Info|State#state.proxies],
			{ok,State#state{proxies=NewProxies},Info}
	end.

% Finds best proxy to use for particular node
findProxy(State)->
	case length(State#state.proxies) of
			0 -> {nil,0};
			Count ->
				Sorted=lists:keysort(#proxyInfo.queue,State#state.proxies),
				[Candidate|_]=Sorted,
				{Candidate,Count}
	end.


handle_cast({decQueue,ProxyPid}, State) ->
	NewState=decQueue(State,ProxyPid),
	{noreply, NewState}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) ->
	% TODO Close all connections
	ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
