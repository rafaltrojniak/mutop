-module(munin_manager).
-author("Rafał Trójniak <rafal@trojniak.net>").
-vsn(0.1).
-behavior(gen_server).
% Starts, registers and return query pools

%gen_server requirements
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state,{
		pools=nil,
		sensors=[]
	}).

-define(WAITING_QUEUE,2).
-define(PROXY_LIMIT,3).
-define(QUERY_TIMEOUT,30100).

%general api
-export([start/0, stop/1]).

%request api
-export([getPool/1, delPool/1, getSensor/2, delSensor/1]).

% These are all wrappers for calls to the server
start() -> gen_server:start_link({local,?MODULE}, ?MODULE, [], []).
stop(_Pid) ->  gen_server:call(?MODULE, stop).

% Restuest api
getPool(Node)  ->
	gen_server:call(?MODULE,{getPool,Node}).
delPool(Node)  ->
	gen_server:call(?MODULE,{delPool,Node}).
% Restuest api
getSensor(Node,Plugin)  ->
	gen_server:call(?MODULE,{getSensor,Node,Plugin}).
delSensor(Pid) when is_pid(Pid)  ->
	gen_server:call(?MODULE,{delSensor,Pid}).

init([]) ->
	{ok, #state{pools=dict:new()}}.

getPool(Node,State) ->
	case dict:find(Node,State#state.pools) of
		error ->
			case munin_client_pool:start(Node) of
				{ok,Pool} ->
					munin_client_pool:start(Node),
					NewDict=dict:store(Node,Pool,State#state.pools),
					{ok,Pool, State#state{pools=NewDict}};
				{error, Reason} ->
					{error,Reason}
			end;
		{ok,Pool} ->
			{ok,Pool, State}
	end.

handle_call({getPool,Node}, _From, State) ->
	case getPool(Node,State) of
		{error,Reason} ->
			{reply,{error,Reason},State};
		{ok,Pool,NewState}->
			{reply, Pool, NewState}
	end;
handle_call({getSensor,Node,Plugin}, _From, State) ->
	case lists:keyfind({Node,Plugin},1,State#state.sensors) of
		false ->
			case getPool(Node,State) of
				{error, Reason} ->
					{reply, {error,Reason},State};
				{ok,Pool,NewState} ->
					case munin_sensor:start(Pool,Plugin) of
						{error,Reason} ->
							{reply, {error,Reason}, NewState};
						{ok,SensorPid} ->
							NewSensors=[{{Node,Plugin},SensorPid}|NewState#state.sensors],
							{reply, {ok,SensorPid}, NewState#state{sensors=NewSensors}}
					end
			end;
		{_,SensorPid}->
			{reply, {ok,SensorPid}, State}
	end;
handle_call({delSensor,Pid}, _From, State) ->
	NewSensors=lists:keydelete(Pid,2,State#state.sensors),
	{reply, ok, State#state{sensors=NewSensors}};
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
