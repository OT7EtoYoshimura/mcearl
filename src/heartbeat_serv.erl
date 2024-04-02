-module(heartbeat_serv).
-export([start_link/2, format/4, salt/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {maxusr, name, salt, users=0}).
-define(SERVER, ?MODULE).
-define(MINUTE, 60*1000).

init([MaxUsers, Name]) ->
	Salt = salt(),
	timer:send_interval(?MINUTE, heartbeat),
	{ok, #state{maxusr=MaxUsers, name=Name, salt=Salt, users=0}}.

start_link(MaxUsers, Name)          -> gen_server:start_link({local, ?SERVER}, ?MODULE, [MaxUsers, Name], []).
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State)            -> {noreply, State}.
handle_info(heartbeat, State = #state{maxusr=MaxUsers, name=Name, salt=Salt, users=Users}) ->
	Url = format(MaxUsers, Name, Salt, Users),
	httpc:request(Url),
	{noreply, State};
handle_info(_Info, State)           -> {noreply, State}.
terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

format(Max, Name, Salt, Users) ->
	Url = uri_string:normalize(#{
		scheme => "http",
		host => "www.classicube.net",
		path => "/heartbeat.jsp"
	}),
	Query = uri_string:compose_query([
		{"port", "25565"},
		{"max", Max},
		{"name", Name},
		{"public", "True"},
		{"version", "7"},
		{"salt", Salt},
		{"users", integer_to_list(Users)}
	]),
	Url ++ "?" ++ Query.

salt() -> lists:sublist(binary_to_list(base62:encode(rand:bytes(64))), 16).
