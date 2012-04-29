-module(munin_client).
-author("Rafał Trójniak <rafal@trojniak.net>").
-vsn(0.2).
-behavior(gen_server).

-define(COM_TIMEOUT,30000).

%gen_server requirements
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state,{
		con=nil,
		plugins=nil
	}).

%general api
-export([start/1, stop/1]).

%request api
-export([list/1, config/2, fetch/2, version/1, cap/1, nodes/1]).

% These are all wrappers for calls to the server
start(Host) -> gen_server:start(?MODULE, Host, []).
stop(Pid) when is_pid(Pid) -> gen_server:call(Pid, stop,?COM_TIMEOUT).

% Restuest api
list(Pid) when is_pid(Pid) ->
	gen_server:call(Pid,list,?COM_TIMEOUT).
cap(Pid) when is_pid(Pid) ->
	gen_server:call(Pid,cap,?COM_TIMEOUT).
nodes(Pid) when is_pid(Pid) ->
	gen_server:call(Pid,nodes,?COM_TIMEOUT).
version(Pid) when is_pid(Pid) ->
	gen_server:call(Pid,version,?COM_TIMEOUT).
config(Pid, Plugin) when is_pid(Pid) and is_list(Plugin) ->
	gen_server:call(Pid,{config,Plugin},?COM_TIMEOUT).
fetch(Pid, Plugin) when is_pid(Pid) and is_list(Plugin) ->
	gen_server:call(Pid,{fetch,Plugin},?COM_TIMEOUT).

% This is called when a connection is made to the server
init(Host) ->
	case munin_con:new(Host) of
		{error, Reason} ->
			{stop,{failedToCreateConnection,Reason}};
		{ok,Con} ->
			{ok,PluginLine}=munin_con:list(Con),
			Plugins=string:tokens(PluginLine," \n"),
			{ok, #state{con=Con,plugins=Plugins}}
	end.

handle_call(nodes, _From, State) ->
	helperGetSingle(nodes,State);
handle_call(version, _From, State) ->
	helperGetSingle(version,State);
handle_call(cap, _From, State) ->
	helperGetSingle(cap,State);

handle_call({config, Plugin}, _From, State) ->
	helperGetPamametrized(config,Plugin,State);
handle_call({fetch, Plugin}, _From, State) ->
	helperGetPamametrized(fetch,Plugin,State);
handle_call(list, _From, State) ->
	{reply, {ok, State#state.plugins}, State};
handle_call(stop, _From, State) ->
	{stop, normal, ok, State}.

helperGetPamametrized(Command,Plugin,State)->
	case lists:member(Plugin,State#state.plugins) of
		true ->
			{ok, Result}= munin_con:Command(State#state.con,Plugin),
			{reply, {Command, Result}, State};
		false ->
			{reply, {error, noPlugin}, State}
	end.
helperGetSingle(Command,State)->
	{ok, Value}= munin_con:Command(State#state.con),
	{reply, {ok, Value}, State}.

% We get compile warnings from gen_server unless we define these
handle_cast(_Message, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, State) -> 
	munin_con:destroy(State#state.con),
	ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
