-module(mcearl_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).
-define(MAXUSR, "32").
-define(NAME, "Testing...").
-define(MOTD, "Test MOTD").
-define(PORT, 25565).
-define(WORLD, "priv/world.cw").

% === %
% API %
% === %
start_link() -> supervisor:start_link({local, ?SERVER}, ?MODULE, []).

% ========= %
% Callbacks %
% ========= %
init([])
	-> SupFlags =
		#{strategy  => one_for_one
		, intensity => 0
		, period    => 1
		}
	,  ChildSpecs =
		[	#{id       => my_pg
			, start    => {pg, start_link, []}
			}
		,	#{id       => world_serv
			, start    => {world_serv, start_link, [?WORLD]}
			, restart  => permanent
			, shutdown => 2000
			, type     => worker
			, modules  => [world_serv]
			}
		,	#{id       => heartbeat_serv
			, start    => {heartbeat_serv, start_link, [?MAXUSR, ?NAME]}
			, restart  => permanent
			, shutdown => 2000
			, type     => worker
			, modules  => [heartbeat_serv]
			}
		,	#{id       => player_man
			, start    => {player_man, start_link, [?PORT]}
			, restart  => permanent
			, shutdown => 2000
			, type     => worker
			, modules  => [player_man]
			}
		,	#{id       => player_sup
			, start    => {player_sup, start_link, [?NAME, ?MOTD]}
			, restart  => permanent
			, shutdown => 2000
			, type     => supervisor
			, modules  => [player_sup]
			}
		]
	,  {ok, {SupFlags, ChildSpecs}}
	.
