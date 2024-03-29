-module(mcearl_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).
-define(MAXUSR, "32").
-define(NAME, "Testing...").

start_link() -> supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	SupFlags = #{strategy => one_for_one
	           , intensity => 0
	           , period => 1
	},
	ChildSpecs = [
		#{id => player_sup
		, start => {player_sup, start_link, []}
		, restart => permanent
		, shutdown => 2000
		, type => supervisor
		, modules => [player_sup]
		},
		#{id => listener_serv
		, start => {listener_serv, start_link, []}
		, restart => permanent
		, shutdown => 2000
		, type => worker
		, modules => [listener_serv]
		},
		#{id => heartbeat_serv
		, start => {heartbeat_serv, start_link, [?MAXUSR, ?NAME]}
		, restart => permanent
		, shutdown => 2000
		, type => worker
		, modules => [heartbeat_serv]
		}
	],
	{ok, {SupFlags, ChildSpecs}}.
