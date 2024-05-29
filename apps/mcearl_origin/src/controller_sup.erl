-module(controller_sup).
-behaviour(supervisor).
-export([start_link/0, start_controller/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

% === %
% API %
% === %
start_link()       -> supervisor:start_link({global, ?SERVER}, ?MODULE, []).
start_controller() -> supervisor:start_child({global, ?SERVER}, []).

% ========= %
% Callbacks %
% ========= %
init([])
	-> SupFlags =
		#{strategy  => simple_one_for_one
		, intensity => 0
		, period    => 1
		}
	,  ChildSpecs =
		[	#{id       => controller
			, start    => {controller, start_link, []}
			, restart  => transient
			, shutdown => 2000
			, type     => worker
			, modules  => [controller]
			}
		]
	,  {ok, {SupFlags, ChildSpecs}}
	.
