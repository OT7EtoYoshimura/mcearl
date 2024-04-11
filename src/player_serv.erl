-module(player_serv).
-behaviour(gen_server).
-export([start_link/2, accept/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {socket, ipaddr, pid, name, motd, id}).
-define(MINUTE, 60*1000).

% === %
% API %
% === %
start_link(Name, MOTD) -> gen_server:start_link(?MODULE, [Name, MOTD], []).
accept(Pid, Socket, Id) -> gen_server:cast(Pid, {accept, Socket, Pid, Id}).

% ========= %
% Callbacks %
% ========= %
init([Name, MOTD]) ->
	process_flag(trap_exit, true),
	pg:join(updates, self()),
	{ok, #state{name=list_to_binary(Name), motd=list_to_binary(MOTD)}}.
handle_call(_Req, _From, State) -> {reply, ok, State}.

handle_cast({pg, Pkt}, #state{socket=Socket} = State) ->
	gen_tcp:send(Socket, Pkt),
	{noreply, State};
handle_cast(ping, #state{socket=Socket} = State) ->
	PingPkt = protocol:build({ping}),
	gen_tcp:send(Socket, PingPkt),
	{noreply, State};
handle_cast({accept, Socket, Pid, Id}, State) ->
	inet:setopts(Socket, [{active, once}, binary]),
	{ok, {IP, _Port}} = inet:peername(Socket),
	timer:apply_repeatedly(?MINUTE, gen_server, cast, [Pid, ping]),
	{noreply, State#state{socket=Socket, ipaddr=IP, pid=Pid, id=Id}};
handle_cast(_Msg, State) -> {noreply, State}.

handle_info({tcp, Socket, Msg}, #state{socket=Socket} = State) ->
	Pkt = protocol:parse(Msg),
	respond(Pkt, State),
	inet:setopts(Socket, [{active, once}]),
	{noreply, State};
handle_info({tcp_closed, Socket}, #state{socket=Socket} = State) ->
	{stop, normal, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(_Rsn, _State)              -> ok.
code_change(_OldVsn, State, _Extra)  -> {ok, State}.

% ========= %
% Utilities %
% ========= %
respond({id, _PlayerName, _VerKey, _IsOp}, #state{socket=Socket, name=Name, motd=MOTD}) ->
	gen_tcp:send(Socket, protocol:build({id, Name, MOTD, false})),
	gen_tcp:send(Socket, protocol:build({lvl_init})),
	{DataPkts, X, Y, Z} = world_serv:data_pkts(),
	lists:map(fun(DataPkt) -> gen_tcp:send(Socket, DataPkt) end, DataPkts),
	gen_tcp:send(Socket, protocol:build({lvl_fin, X, Y, Z})) ;
respond({set_block_m, X, Y, Z, _Mode, BlockType}, _State) ->
	pg_send(protocol:build({set_block, X, Y, Z, BlockType}));
respond({pos_and_orient, _PlayerId, {XInt, XFrac}, {YInt, YFrac}, {ZInt, ZFrac}, Yaw, Heading}, #state{id=Id}) ->
	pg_send(protocol:build({pos_and_orient, Id, {XInt, XFrac}, {YInt, YFrac}, {ZInt, ZFrac}, Yaw, Heading}));
respond({msg, _PlayerId, Msg}, #state{id=Id}) ->
	pg_send(protocol:build({msg, Id, Msg}));
respond(undefined, _State) -> ok.

pg_send(Pkt) ->
	lists:map(
		fun(Pid) -> gen_server:cast(Pid, {pg, Pkt}) end,
		lists:delete(self(), pg:get_members(updates))
	).
