-module(mcearl_peer_app).
-behaviour(application).
-export([start/2, stop/1]).

% ========= %
% Callbacks %
% ========= %
start(_StartType, _StartArgs) -> mcearl_peer_sup:start_link().
stop(_State)                  -> ok.
