-module(mcearl_app).
-behaviour(application).
-export([start/2, stop/1]).

% ========= %
% Callbacks %
% ========= %
start(_StartType, _StartArgs) -> mcearl_sup:start_link().
stop(_State)                  -> ok.
