-module(munin_manager).
-author("Rafał Trójniak <rafal@trojniak.net>").
-vsn(0.1).
-behavior(gen_server).
% Starts, registers and return query pools

%gen_server requirements
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state,{
		pools=nil
	}).

-define(WAITING_QUEUE,2).
-define(PROXY_LIMIT,3).
-define(QUERY_TIMEOUT,30100).

%general api
-export([start/0, stop/1]).

%request api
-export([getPool/2, delPool/2]).

% These are all wrappers for calls to the server
start() -> gen_server:start_link({local,?MODULE}, ?MODULE, [], []).
stop(_Pid) ->  gen_server:call(?MODULE, stop).

% Restuest api
getPool(_Pid, Node)  ->
	gen_server:call(?MODULE,{getPool,Node}).
delPool(_Pid, Node)  ->
	gen_server:call(?MODULE,{delPool,Node}).

init([]) ->
	{ok, #state{pools=dict:new()}}.

handle_call({getPool,Node}, _From, State) ->
	case dict:find(Node,State#state.pools) of
		error ->
			{ok,Pool}=munin_client_pool:start(Node),
			NewDict=dict:store(Node,Pool,State#state.pools),
			{reply, Pool, State#state{pools=NewDict}};
		{ok,Pool} ->
			{reply, Pool, State}
	end;
handle_call({delPool,Node}, _From, State) ->
	NewPools=dict:erase(Node,State#state.pools),
	{reply, ok, State#state{pools=NewPools}};
handle_call(stop, _From, State) ->
	{stop, normal, ok, State}.


% We get compile warnings from gen_server unless we define these
handle_cast(missing, State) ->
	{noreply, State}.
handle_info(missing, State) ->
	{noreply, State}.
terminate(_Reason, _State) ->
	% TODO Close all pools
	ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
