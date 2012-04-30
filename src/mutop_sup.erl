
-module(mutop_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
					{munin_manager,
						{munin_manager,start,[]},
						permanent,
						5000,
						worker,
						[]},
					{escreen_gate,
						{escreen_gate,start,[]},
						permanent,
						5000,
						worker,
						[]},
					{mutop,
						{mutop_controller,start,[]},
						permanent,
						5000,
						worker,
						[]}
				]} }.
