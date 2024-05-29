-module(mcearl_origin_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).
-define(MAXUSR, "32").
-define(NAME, "New server implementation test (very unstable)").
-define(MOTD, "Test MOTD").
-define(PORT, 25565).
-define(WORLD, "world.cw").

% === %
% API %
% === %
start_link() -> supervisor:start_link({global, ?SERVER}, ?MODULE, []).

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
		[	#{id       => heartbeat
			, start    => {heartbeat, start_link, [?MAXUSR, ?NAME]}
			, restart  => permanent
			, shutdown => 2000
			, type     => worker
			, modules  => [heartbeat]
			}
		,	#{id       => world
			, start    => {world, start_link, [?WORLD]}
			, restart  => permanent
			, shutdown => 2000
			, type     => worker
			, modules  => [world]
			}
		,	#{id       => controller_sup
			, start    => {controller_sup, start_link, []}
			, restart  => permanent
			, shutdown => 2000
			, type     => supervisor
			, modules  => [controller_sup]
			}
		,	#{id       => controller_man
			, start    => {controller_man, start_link, []}
			, restart  => permanent
			, shutdown => 2000
			, type     => worker
			, modules  => [controller_man]
			}
		]
	,  {ok, {SupFlags, ChildSpecs}}
	.
