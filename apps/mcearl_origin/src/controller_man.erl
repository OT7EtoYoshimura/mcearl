-module(controller_man).
-behaviour(gen_server).
-export([start_link/0, get_server/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {peers}).
-define(SERVER, ?MODULE).

% === %
% API %
% === %
start_link() -> gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).
get_server(Ip, Proxy) -> gen_server:call({global, ?SERVER}, {get_server, Ip, Proxy}, 20000).

% ========= %
% Callbacks %
% ========= %
init([]) -> {ok, #state{peers=[]}}.

handle_call({get_server, Ip, Proxy}, _From, #state{peers=Peers})
	-> {Pid, Cnt, ToAdd} =
		case lists:partition(fun({_Pid, Cnt}) -> Cnt < 255 end, Peers) of
		{[], Unsatif}
			-> {ok, ContPid} = controller_sup:start_controller()
			,  {ContPid, 1, Unsatif}
			;
		{[{ContPid, ContCnt} | Rest], Unsatif} -> {ContPid, ContCnt + 1, Rest ++ Unsatif}
		end
	,  ServPid = controller:get_server(Pid, Ip, Proxy)
	,  {reply, ServPid, #state{peers=[{Pid, Cnt} | ToAdd]}}
	;
handle_call(_Req, _From, State)     -> {reply, ok, State}.

handle_cast(_Msg, State)            -> {noreply, State}.
handle_info(_Info, State)           -> {noreply, State}.
terminate(_Rsn, _State)             -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
