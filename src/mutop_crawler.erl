-module(mutop_crawler).
-author("Rafał Trójniak <rafal@trojniak.net>").
-vsn(0.1).
-behavior(gen_fsm).

%gen_fsm requirements
-export([init/1, terminate/3]).

-record(state,{
		hosts=[],
		allPlugins=nil,
		byHost=[]
	}).


%general api
-export([start/0, stop/0]).

% States
-export([discover/2, populate/2]).

start() -> gen_fsm:start_link({local,?MODULE}, ?MODULE, [], []).
stop() ->  gen_fsm:call(?MODULE, stop).

init([]) ->
	case application:get_env(mutop, hosts) of
		undefined ->
			Hosts=[ {localhost, "localhost"} ];
		{ok, Hosts} -> Hosts
	end,
	mutop_controller:setInfo("Detecting plugins"),
	{ok, discover,  #state{hosts=Hosts, allPlugins=sets:new()},0}.

discover(timeout, State)->
	case State#state.hosts of
		[] -> {next_state, populate, State,0};
		[{Name,Address}|HostTail] ->
			mutop_controller:setInfo("Detecting plugins"++ atom_to_list(Name)),
			Pool=munin_manager:getPool(Address),
			{ok,PluginList}=munin_client_pool:list(Pool),
			mutop_controller:setInfo("Got plugins from "++ atom_to_list(Name)),
			NewByHost=[{Name, PluginList}|State#state.byHost],
			NewAllPlugins=sets:union(State#state.allPlugins,
				sets:from_list(PluginList)),
			{next_state, discover, State#state{
					hosts=HostTail,
					byHost=NewByHost,
					allPlugins=NewAllPlugins},0}
	end.

populate(timeout, State)->
	mutop_controller:setInfo("Generating sensor "),
	%io:format("All plugins:~p\n",[sets:to_list(State#state.allPlugins)]),
	%io:format("byHost :~p\n",[State#state.byHost]),
	{stop, normal, State}.

terminate(_Reason, _State, _Data) ->
	ok.
