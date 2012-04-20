-module(munin_client_manager).
-author("Rafał Trójniak <rafal@trojniak.net>").
-vsn(0.1).
-behavior(gen_server).
% This module allows to call any action on any host
% It also:
% - creates connections to hosts and keeps track of them
% - monitors connection pool
% - blance requests to the connections

%gen_server requirements
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state,{
		proxies=[]
	}).
-record(proxyInfo,{
		proxy=nil,
		queue=0
	}).

-define(WAITING_QUEUE,2).
-define(PROXY_LIMIT,3).
-define(QUERY_TIMEOUT,30100).

%general api
-export([start/0, stop/1]).

%request api
-export([list/2, config/3, fetch/3, version/2, cap/2, nodes/2]).

%API for proxy
-export([requestDone/3]).

% These are all wrappers for calls to the server
start() -> gen_server:start_link(?MODULE, [], []).
stop(Pid) when is_pid(Pid) -> gen_server:call(Pid, stop).

% Restuest api
list(Pid, Node) when is_pid(Pid) ->
	gen_server:call(Pid,{list,Node},?QUERY_TIMEOUT).
cap(Pid, Node) when is_pid(Pid) ->
	gen_server:call(Pid,{cap,Node},?QUERY_TIMEOUT).
nodes(Pid, Node) when is_pid(Pid) ->
	gen_server:call(Pid,{nodes,Node},?QUERY_TIMEOUT).
version(Pid, Node) when is_pid(Pid) ->
	gen_server:call(Pid,{version,Node},?QUERY_TIMEOUT).
config(Pid, Node, Plugin) when is_pid(Pid) and is_list(Plugin) ->
	gen_server:call(Pid,{{config,Plugin},Node},?QUERY_TIMEOUT).
fetch(Pid, Node, Plugin) when is_pid(Pid) and is_list(Plugin) ->
	gen_server:call(Pid,{{fetch,Plugin},Node},?QUERY_TIMEOUT).

% Proxy api
requestDone(MyPid,Node,ProxyPid) when is_pid(MyPid) and is_pid(ProxyPid) ->
	gen_server:cast(MyPid,{decQueue,Node,ProxyPid}).

init([]) ->
	{ok, #state{proxies=dict:new()}}.

handle_call({Query,Node}, From, State) ->
	{ProxyInfo,Count} = findProxy(State,Node),
	if
		ProxyInfo == nil ->
			{BigerState,PickedProxy}=addProxy(State,Node);
		Count < ?PROXY_LIMIT andalso ProxyInfo#proxyInfo.queue >= ?WAITING_QUEUE ->
			{BigerState,PickedProxy}=addProxy(State,Node);
		true ->
			PickedProxy=ProxyInfo,
			BigerState=State
	end,
	IncreasedState=incQueue(BigerState,Node,PickedProxy),
	munin_client_proxy:call(PickedProxy#proxyInfo.proxy,From,Query),
	{noreply, IncreasedState};
handle_call(stop, _From, State) ->
	{stop, normal, ok, State}.

% Increases queue length for ProxyInfo (Info) and Node, returns new state
incQueue(State,Node,Info) ->
	IncInfo=Info#proxyInfo{queue=Info#proxyInfo.queue+1},
	OldList=dict:fetch(Node,State#state.proxies),
	NewList=lists:keyreplace(IncInfo#proxyInfo.proxy,
		#proxyInfo.proxy,
		OldList,
		IncInfo),
	State#state{proxies=dict:store(Node,NewList,State#state.proxies)}.

% Decreases queue length for ProxyPid proxy and Node, returns new state
decQueue(State,Node,ProxyPid) ->
	OldList=dict:fetch(Node,State#state.proxies),
	{value,OldInfo,ShorterList}=lists:keytake(ProxyPid,#proxyInfo.proxy,OldList),
	NewInfo=OldInfo#proxyInfo{queue=OldInfo#proxyInfo.queue-1},
	NewList=[NewInfo|ShorterList],
	State#state{proxies=dict:store(Node,NewList,State#state.proxies)}.

% Adds new proxy process to the sate
addProxy(State,Node) ->
	{ok,Proxy}=munin_client_proxy:start(Node,self()),
	Info=#proxyInfo{proxy=Proxy,queue=0},
	NewProxies=dict:append( Node, Info, State#state.proxies),
	{State#state{proxies=NewProxies},Info}.

% Finds best proxy to use for particular node
findProxy(State, Node)->
	case dict:find(Node,State#state.proxies) of
		error -> {nil,0};
		{ok,List} ->
			case length(List) of
					0 -> {nil,0};
					Count ->
						Sorted=lists:keysort(#proxyInfo.queue,List),
						[Candidate|_]=Sorted,
						{Candidate,Count}
			end
	end.


% We get compile warnings from gen_server unless we define these
handle_cast({decQueue,Node,ProxyPid}, State) ->
	NewState=decQueue(State,Node,ProxyPid),
	{noreply, NewState}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) ->
	% TODO Close all connections
	ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
