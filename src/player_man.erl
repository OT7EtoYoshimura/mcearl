-module(player_man).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {taken=[], avail=lists:delete(-1, lists:seq(127, -128, -1))}).
-define(SERVER, ?MODULE).

% === %
% API %
% === %
start_link(Port) -> gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

% ========= %
% Callbacks %
% ========= %
init([Port])
	-> process_flag(trap_exit, true)
	,  Opts =
		[ binary
%		, {packet, 2}
		, {reuseaddr, true}
		, {keepalive, true}
		, {backlog, 30}
		, {active, false}
		]
	,  case gen_tcp:listen(Port, Opts) of
		{ok, ListenSocket} -> spawn_link(fun() -> listen(ListenSocket) end);
		{error, Rsn}       -> {stop, Rsn}
	end
	,  {ok, #state{}}
	.

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast({accept, AcceptSocket}, #state{taken=Taken, avail=Avail} = State)
	-> {ok, Pid} = player_sup:start_player()
	,  gen_tcp:controlling_process(AcceptSocket, Pid)
	,  {[Id], NewAvail} = lists:split(1, Avail)
	,  player_serv:accept(Pid, AcceptSocket, Id)
	,  Ref = monitor(process, Pid)
	,  {noreply, State#state{taken=[{Ref, Id}|Taken], avail=NewAvail}}
	;
handle_cast(_Msg, State)            -> {noreply, State}.
handle_info({'DOWN', Ref, process, _Pid, _Info}, #state{taken=Taken, avail=Avail} = State)
	-> Id = proplists:get_value(Ref, Taken)
	,  NewTaken = proplists:delete(Ref, Taken)
	,  {noreply, State#state{taken=NewTaken, avail=[Id|Avail]}}
	;
handle_info(_Info, State)           -> {noreply, State}.
terminate(_Rsn, _State)             -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% ========= %
% Utilities %
% ========= %
listen(ListenSocket)
	-> case gen_tcp:accept(ListenSocket) of
		{ok, AcceptSocket} ->
			gen_tcp:controlling_process(AcceptSocket, whereis(?SERVER)),
			gen_server:cast(?SERVER, {accept, AcceptSocket}),
			listen(ListenSocket);
		{error, Rsn} -> {stop, Rsn}
	end
	.
