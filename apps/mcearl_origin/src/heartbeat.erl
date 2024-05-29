-module(heartbeat).
-behaviour(gen_server).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {maxusr, name, salt, users=0}).
-define(SERVER, ?MODULE).
-define(MINUTE, 60*1000).

% === %
% API %
% === %
start_link(MaxUsers, Name) -> gen_server:start_link({global, ?SERVER}, ?MODULE, [MaxUsers, Name], []).

% ========= %
% Callbacks %
% ========= %
init([MaxUsers, Name])
	-> timer:apply_interval(?MINUTE, gen_server, cast, [self(), heartbeat])
	,  {ok, #state{maxusr=MaxUsers, name=Name, salt=salt(), users=0}}
	.

handle_call(_Req, _From, State)     -> {reply, ok, State}.

handle_cast(heartbeat, #state{maxusr=MaxUsers, name=Name, salt=Salt, users=Users} = State)
	-> Url = format(MaxUsers, Name, Salt, Users)
	,  httpc:request(Url)
	,  {noreply, State}
	;
handle_cast(alive, #state{users=Users} = State) -> {noreply, State#state{users=Users+1}};
handle_cast(dead , #state{users=Users} = State) -> {noreply, State#state{users=Users-1}};
handle_cast(_Msg, State)            -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Rsn, _State)             -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% ========= %
% Utilities %
% ========= %
format(Max, Name, Salt, Users)
	-> Url = uri_string:normalize(
		#{scheme => "http"
		, host   => "www.classicube.net"
		, path   => "/heartbeat.jsp"
		}
	)
	,  Query = uri_string:compose_query(
		[ {"port",    "25565"}
		, {"max",     Max}
		, {"name",    Name}
		, {"public",  "True"}
		, {"version", "7"}
		, {"salt",    Salt}
		, {"users",   integer_to_list(Users)}
		]
	)
	,  Url ++ "?" ++ Query
	.

salt() -> lists:sublist(binary_to_list(base62:encode(rand:bytes(64))), 16).
