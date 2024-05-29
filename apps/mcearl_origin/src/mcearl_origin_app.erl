-module(mcearl_origin_app).
-behaviour(application).
-export([start/2, stop/1]).

% ========= %
% Callbacks %
% ========= %
start(_StartType, _StartArgs)
	-> {ok, _} = ranch:start_listener(proxy, ranch_tcp, #{num_acceptors => 100, socket_opts => [{port, 25565}]}, proxy, [])
	,  application:load(mcearl_peer)
	,  application:load(erl_nbt)
	,  mcearl_origin_sup:start_link().
stop(_State)                  -> ok.
