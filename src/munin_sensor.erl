-module(munin_sensor).
-author("Rafał Trójniak <rafal@trojniak.net>").
-vsn(0.1).
-behavior(gen_server).
% Starts, registers and return query pools

%gen_server requirements
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state,{
		conModule=nil,
		conPid=nil,
		name=nil,
		config=nil
	}).

-record(field,{
		type=gauge,
		name=nil,
		label="",
		state=nil,
		stamp=nil
	}).
-record(config,{
		fields=[],
		general=[]
	}).

%general api
-export([start/2, stop/1]).

%request api
-export([getFields/1, getValues/1, getConfig/1]).

% These are all wrappers for calls to the server
start({pool,PoolPid},Name) when is_pid(PoolPid) ->
	gen_server:start_link(?MODULE, {{munin_client_pool, PoolPid}, Name}, [{debug,[trace,log]}]);
start({client,ClientPid},Name) when is_pid(ClientPid) ->
	gen_server:start_link(?MODULE, {{munin_client, ClientPid}, Name}, [{debug,[trace]}]).

stop(Pid) when is_pid(Pid) ->
	gen_server:call(Pid, stop).

% Restuest api
getFields(Pid) when is_pid(Pid) ->
	gen_server:call(Pid,getFields).

getValues(Pid) when is_pid(Pid) ->
	gen_server:call(Pid,getValues).

getConfig(Pid) when is_pid(Pid) ->
	gen_server:call(Pid,getConfig).

% Initiation
init({{ConModule,ConPid},Name}) ->
	{config,ConfigData}=ConModule:config(ConPid,Name),
	Config=parseConfigData(ConfigData,#config{}),
	% Get and parse config
	{ok, #state{conModule=ConModule,conPid=ConPid,name=Name,config=Config}}.

handle_call(getFields, _From, State) ->
	Fields=lists:map(fun(FieldConfig) -> FieldConfig#field.name end, State#state.config#config.fields),
	{reply, Fields, State};
handle_call(getValues, _From, State) ->
	ConModule=State#state.conModule,
	{fetch,{Fetch,Stamp}}=ConModule:fetch(State#state.conPid,State#state.name),
	{NewFields, Values}=parseFetch(Fetch,Stamp,State#state.config#config.fields),
	NewConfig=State#state.config#config{fields=NewFields},
	{reply, Values, State#state{config=NewConfig}};
handle_call(getConfig, _From, State) ->
	{reply, State#state.config#config.general, State};
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

% Parses config data returned from the node
parseConfigData([["graph_title "|Title]|ConfigTail], Config) ->
	General=lists:keystore(title,1,Config#config.general, {title,string:strip(Title)}),
	parseConfigData(ConfigTail, Config#config{general=General});

parseConfigData([ConfigLine|ConfigTail], Config) ->
	case string:chr(ConfigLine,$ ) of
		0->
				% No space in the configLine
				io:format("~p:~w Skiping Line ~s\n",[self(),?LINE,ConfigLine]),
				parseConfigData(ConfigTail, Config);
		Space ->
			{Header," " ++ Value}=lists:split(Space-1,ConfigLine),
			case string:chr(Header,$.) of
				0 ->
					io:format("~p:~w Skiping Line ~s\n",[self(),?LINE,ConfigLine]),
					parseConfigData(ConfigTail, Config);
				_ ->
					case string:chr(Header,$.) of
						0 ->
							% no field separator - skipping
							io:format("~p:~w Skiping Line ~s\n",[self(),?LINE,ConfigLine]),
							parseConfigData(ConfigTail, Config);
						DotPosition ->
							{Field,"."++ Property}=lists:split(DotPosition-1,Header),
							case lists:keysearch(Field,#field.name,Config#config.fields) of
								false ->
									FieldConfig=parsePropertyLine(Property,Value,#field{name=Field});
								{value,ConfigFound} ->
									FieldConfig=parsePropertyLine(Property,Value,ConfigFound)
							end,
							Fields=lists:keystore(Field, #field.name, Config#config.fields, FieldConfig),
							parseConfigData(ConfigTail, Config#config{fields=Fields})
					end
			end
	end;

parseConfigData([], Config) ->
	Config.

% Parses property for signle field
parsePropertyLine("type","COUNTER",Config) ->
	Config#field{type=counter};
parsePropertyLine("type","ABSOLUTE",Config) ->
	Config#field{type=absolute};
parsePropertyLine("type","DERIVE",Config) ->
	Config#field{type=derive};
parsePropertyLine("type","GAUGE",Config) ->
	Config#field{type=gauge};
parsePropertyLine("label",Label,Config) ->
	Config#field{label=Label};
parsePropertyLine(_Property,_Label,Config) ->
	Config.

parseFetch(Fetch,Stamp,Configs)->
	parseFetch(Fetch,Stamp,Configs,[]).
parseFetch([],_Stamp,Configs,Fields)->
	{Configs,Fields};
parseFetch([Line|FetchTail],Stamp,Configs,Fields)->
	case string:chr(Line,$ ) of
		0->
			% No space found
			parseFetch(FetchTail,Stamp,Configs,Fields);
		SpacePosition ->
			{Header," "++Value} = lists:split(SpacePosition-1, Line),
			case string:chr(Header,$.) of
				0 ->
					% No dot found
					parseFetch(FetchTail,Stamp,Configs,Fields);
				DotPosition ->
					{FieldName,".value"} = lists:split(DotPosition-1, Header),
					case lists:keysearch(FieldName,#field.name,Configs) of
						false -> 
							% No configuration found for such field name
							parseFetch(FetchTail,Stamp,Configs,Fields);
						{value, FieldConfig} ->
							{NewConfig,ParsedValue}=parseByConfig(castToFloat(Value), Stamp, FieldConfig),
							NewFields=[{FieldName,ParsedValue}|Fields],
							NewConfigs=lists:keystore(FieldName,#field.name,Configs, NewConfig),
							parseFetch(FetchTail,Stamp,NewConfigs,NewFields)
					end
			end
	end.

parseByConfig(Value, _Stamp, FieldConfig)
		when FieldConfig#field.type==absolute ->
	{FieldConfig,Value};

parseByConfig(Value, Stamp, FieldConfig)
		when FieldConfig#field.type==guage->
	{FieldConfig#field{stamp=Stamp,state=Value},Value}; %TODO handle type

parseByConfig(Value, Stamp, FieldConfig)
		when FieldConfig#field.type == counter , FieldConfig#field.state == nil ->
	{FieldConfig#field{stamp=Stamp,state=Value}, 0};
parseByConfig(Value, Stamp, FieldConfig)
		when FieldConfig#field.type == counter ->
	{FieldConfig#field{stamp=Stamp,state=Value}, 
		(Value-FieldConfig#field.state)/timeDiffSeconds(Stamp,FieldConfig#field.stamp)};

parseByConfig(Value, Stamp, FieldConfig)
		when FieldConfig#field.type == derive , FieldConfig#field.state == nil ->
	{FieldConfig#field{stamp=Stamp,state=Value}, Value};
parseByConfig(Value, Stamp, FieldConfig)
		when FieldConfig#field.type == derive ->
	{FieldConfig#field{stamp=Stamp,state=Value},
		(Value-FieldConfig#field.state)/timeDiffSeconds(Stamp,FieldConfig#field.stamp)};

parseByConfig(Value, _Stamp, FieldConfig) ->
	{FieldConfig,Value}.

castToFloat(Value) when is_list(Value)->
	case string:to_float(Value) of
		{error,_} ->
			case string:to_integer(Value) of
				{error,_} -> nan;
				{IntVal,_} -> float(IntVal)
			end;
		{NumVal,_}->
			NumVal
	end.

timeDiffSeconds({Mega1,Sec1,Min1},{Mega2,Sec2,Min2}) ->
	(Mega1-Mega2)*1000000+(Sec1-Sec2)+(Min1-Min2)/1000000.
