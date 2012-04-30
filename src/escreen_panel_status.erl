-module(escreen_panel_status).
-author("Rafał Trójniak <rafal@trojniak.net>").
-vsn(0.1).
-behavior(gen_server).
% Starts, registers and return query pools

%gen_server requirements
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state,{
		width=nil,
		height=nil,
		startX=nil,
		startY=nil,
		window=nil,
		content=[]
	}).

%general api
-export([start/4, stop/1]).

%request api
-export([setContent/2]).

% These are all wrappers for calls to the server
start(full, Height, StartX, StartY)
		when  is_integer(Height) and is_integer(StartX) and is_integer(StartX)->
	gen_server:start_link(?MODULE, {full, Height, StartX, StartY}, []);
start(Width, Height, StartX, StartY)
		when is_integer(Width) and is_integer(Height) and is_integer(StartX) and is_integer(StartX)->
	gen_server:start_link(?MODULE, {Width, Height, StartX, StartY}, []).

stop(_Pid) ->  gen_server:call(?MODULE, stop).

% Restuest api
setContent(Pid, Content) when is_pid(Pid) and is_list(Content) ->
	gen_server:call(Pid, {setContent, Content}).

init({full, Height, StartX, StartY})
		when is_integer(Height) and is_integer(StartX) and is_integer(StartX)->
	{X,_}=encurses:getmaxxy(),
	Win=encurses:newwin(X, Height, StartX, StartY),
	{ok, #state{width=full, height=Height, startX=StartX, startY=StartY, window=Win}};
init({Width, Height, StartX, StartY})
		when is_integer(Width) and is_integer(Height) and is_integer(StartX) and is_integer(StartX)->
	Win=encurses:newwin(Width, Height, StartX, StartY),
	{ok, #state{width=Width, height=Height, startX=StartX, startY=StartY, window=Win}}.


handle_call({setContent,Content}, _From, State) ->
	encurses:erase(State#state.window),
	encurses:waddstr(State#state.window, Content),
	encurses:refresh(State#state.window),
	{reply, ok, State#state{content=Content}};
handle_call(stop, _From, State) ->
	{stop, normal, ok, State}.


% We get compile warnings from gen_server unless we define these
handle_cast(missing, State) ->
	{noreply, State}.
handle_info(missing, State) ->
	{noreply, State}.
terminate(_Reason, State) ->
	encurses:delwin(State#state.window),
	ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
