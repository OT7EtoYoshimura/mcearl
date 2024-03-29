-module(player_sup).
-behaviour(supervisor).
-export([start_player/0, start_link/0, init/1]).
-define(SERVER, ?MODULE).

start_player() -> supervisor:start_child(?SERVER, []).
start_link() -> supervisor:start_link({local, ?SERVER}, ?MODULE, []).
init([]) ->
	SupFlags = #{strategy => simple_one_for_one
	           , intensity => 0
	           , period => 1
	},
	ChildSpecs = [
		#{id => player_serv
		, start => {player_serv, start_link, []}
		, restart => transient
		, shutdown => 2000
		, type => worker
		, modules => [player_serv]
		}
	],
	{ok, {SupFlags, ChildSpecs}}.
