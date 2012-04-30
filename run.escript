#!/usr/bin/env escript
%%! -noinput -config config -pa ./ebin +A 10
-include_lib("./include/encurses.hrl").
main(Args) ->
	mutop_app:cli_start(Args).
