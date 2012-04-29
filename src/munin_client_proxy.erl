-module(munin_client_proxy).
-author("Rafał Trójniak <rafal@trojniak.net>").
-vsn(0.1).
-behavior(gen_server).


%gen_server requirements
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state,{
		client=nil,
		host=nil,
		master=nil
	}).

%general api
-export([start/2, stop/1]).

%request api
-export([call/3]).

% These are all wrappers for calls to the server
start(Host, Master) when is_pid(Master) ->
	gen_server:start(?MODULE, {Host,Master}, []).
stop(Pid) when is_pid(Pid) -> gen_server:call(Pid, stop).

% Just call instance
call(Pid, From, Call) when is_pid(Pid)  ->
	gen_server:cast(Pid,{call,Call,From}).

% This is called when a connection is made to the server
init({Host,Master}) ->
	case munin_client:start(Host) of
		{error,Reason} ->
			{stop, Reason};
		{ok,Client}->
			{ok, #state{client=Client,master=Master,host=Host}}
	end.

% Stops execution
handle_call(stop, _From, State) ->
	{stop, normal, ok, State}.

% Runs call to the client
handle_cast({call,{Call,Plugin},From}, State) ->
	Result=munin_client:Call(State#state.client,Plugin),
	handleReply(From,Result,State);
handle_cast({call,Call,From}, State) ->
	Result=munin_client:Call(State#state.client),
	handleReply(From,Result,State).

% Handles call result - sends reply and notification about done request
handleReply(From,Result,State) ->
	gen_server:reply(From,Result),
	munin_client_pool:requestDone(State#state.master,self()),
	{noreply, State}.

handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, State) ->
	munin_client:stop(State#state.client),
	ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
