-module(mutop_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, cli_start/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
		application:start(sasl),
		mutop_sup:start_link().

stop(_State) ->
    ok.

cli_start(_Args) ->
	application:start(mutop),
	receive
		stop -> ok
	end .
