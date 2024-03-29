-module(player_serv).
-behaviour(gen_server).
-export([start_link/0, accept/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {socket, ipaddr}).
-define(SERVER, ?MODULE).

start_link() -> gen_server:start_link(?MODULE, [], []).
init([]) ->
	process_flag(trap_exit, true),
	{ok, #state{}}.
handle_call(_Request, _From, State)  -> {reply, ok, State}.
handle_cast({accept, Socket}, State) ->
	inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),
	{ok, {IP, _Port}} = inet:peername(Socket),
	{noreply, #state{socket=Socket, ipaddr=IP}};
handle_cast(_Msg, State)            -> {noreply, State}.
handle_info(_Info, State)           -> {noreply, State}.
terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

accept(Pid, Socket) ->
	gen_server:cast(self(), {accept, Socket}).
