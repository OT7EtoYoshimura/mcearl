-module(player_sup).
-behaviour(supervisor).
-export([start_player/0, start_link/2, init/1]).
-define(SERVER, ?MODULE).

% === %
% API %
% === %
start_player()         -> supervisor:start_child(?SERVER, []).
start_link(Name, MOTD) -> supervisor:start_link({local, ?SERVER}, ?MODULE, [Name, MOTD]).

% ========= %
% Callbacks %
% ========= %
init([Name, MOTD])
	-> SupFlags =
		#{strategy  => simple_one_for_one
		, intensity => 0
		, period    => 1
		}
	,  ChildSpecs = [
		#{id       => player_serv
		, start    => {player_serv, start_link, [Name, MOTD]}
		, restart  => transient
		, shutdown => 2000
		, type     => worker
		, modules  => [player_serv]
		}
	]
	,  {ok, {SupFlags, ChildSpecs}}.
