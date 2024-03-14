-module(heartbeat).
-compile(export_all).
-define(MINUTE, 60*1000).

apply_interval() ->
	Salt = salt(),
	Url = format("32", "Testing", Salt, "0"),
	timer:apply_interval(?MINUTE, ?MODULE, ?FUNCTION_NAME, []).

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
		{"users", Users}
	]),
	Url ++ "?" ++ Query.
	% uri_string:resolve(Query, Url).

salt() -> lists:sublist(binary_to_list(base62:encode(rand:bytes(64))), 16).
