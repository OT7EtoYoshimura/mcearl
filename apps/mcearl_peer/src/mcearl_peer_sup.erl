-module(mcearl_peer_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).
-define(NAME, "New server implementation test (very unstable)").
-define(MOTD, "Test MOTD").
-define(PORT, 25565).
-define(WORLD, "world.cw").

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
			, start    => {pg, start_link, [node()]}
			}
		,	#{id       => server_sup
			, start    => {server_sup, start_link, [?NAME, ?MOTD]}
			, restart  => permanent
			, shutdown => 2000
			, type     => supervisor
			, modules  => [server_sup]
			}
		,	#{id       => server_man
			, start    => {server_man, start_link, []}
			, restart  => permanent
			, shutdown => 2000
			, type     => worker
			, modules  => [server_man]
			}
		]
	,  {ok, {SupFlags, ChildSpecs}}
	.
