-module(server_sup).
-behaviour(supervisor).
-export([start_server/0, start_link/2]).
-export([init/1]).
-define(SERVER, ?MODULE).

% === %
% API %
% === %
start_link(Name, MOTD) -> supervisor:start_link({local, ?SERVER}, ?MODULE, [Name, MOTD]).
start_server()         -> supervisor:start_child(?SERVER, []).

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
		#{id       => server
		, start    => {server, start_link, [Name, MOTD]}
		, restart  => transient
		, shutdown => 2000
		, type     => worker
		, modules  => [server]
		}
	]
	,  {ok, {SupFlags, ChildSpecs}}.
