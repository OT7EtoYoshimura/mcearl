-module(listener_serv).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).
-define(SERVER, ?MODULE).
-define(PORT, 25565).

init([]) ->
	Opts = [ binary
	       , {packet, 2}
	       , {reuseaddr, true}
	       , {keepalive, true}
	       , {backlog, 30}
	       , {active, false}
	],
	case gen_tcp:listen(?PORT, Opts) of
		{ok, ListenSocket} -> spawn_link(fun() -> listen(ListenSocket) end);
		{error, Rsn}       -> {stop, Rsn}
	end,
	{ok, #state{}}.

listen(ListenSocket) ->
	case gen_tcp:accept(ListenSocket) of
		{ok, AcceptSocket} ->
			{ok, Pid} = player_sup:start_player(),
			gen_tcp:controlling_process(AcceptSocket, Pid),
			player_serv:accept(Pid, AcceptSocket),
			listen(ListenSocket);
		{error, Rsn} -> {stop, Rsn}
	end.

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State)            -> {noreply, State}.
handle_info(_Info, State)           -> {noreply, State}.
terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
