-module(player_serv).
-behaviour(gen_server).
-export([start_link/2, accept/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {socket, ip, pid, name, motd, id, username, pos}).
-define(MINUTE, 60*1000).

% === %
% API %
% === %
start_link(Name, MOTD)  -> gen_server:start_link(?MODULE, [Name, MOTD], []).
accept(Pid, Socket, Id) -> gen_server:cast(Pid, {accept, Socket, Pid, Id}).

% ========= %
% Callbacks %
% ========= %
init([Name, MOTD])
	-> process_flag(trap_exit, true)
	,  pg:join(updates, self())
	,  {ok, #state{name=list_to_binary(Name), motd=list_to_binary(MOTD)}}
	.

handle_call({pg, spawned, SpawnPkt, MsgPkt}, From, #state{socket=Socket, id=Id, username=UserName, pos=Pos} = State)
	when Pos =/= undefined
	-> gen_server:reply(From, {Id, UserName, Pos})
	,  gen_tcp:send(Socket, SpawnPkt)
	,  gen_tcp:send(Socket, MsgPkt)
	,  {noreply, State}
	;
handle_call(_Req, _From, State) -> {reply, ok, State}.

handle_cast({pg, Pkt}, #state{socket=Socket} = State)
	-> gen_tcp:send(Socket, Pkt)
	,  {noreply, State}
	;
handle_cast({accept, Socket, Pid, Id}, State)
	-> inet:setopts(Socket, [{active, once}, binary])
	,  {ok, {IP, _Port}} = inet:peername(Socket)
	,  timer:apply_repeatedly(?MINUTE, gen_tcp, send, [Socket, protocol_lib:build({ping})])
	,  {noreply, State#state{socket=Socket, ip=IP, pid=Pid, id=Id}}
	;
handle_cast(_Msg, State) -> {noreply, State}.

handle_info({tcp, Socket, Msg}, #state{socket=Socket} = State)
	-> NewState = respond(protocol_lib:parse(Msg), State)
	,  inet:setopts(Socket, [{active, once}])
	,  {noreply, NewState}
	;
handle_info({tcp_closed, Socket}, #state{socket=Socket} = State) -> {stop, normal, State};
handle_info(_Info, State)                                        -> {noreply, State}.

terminate(_Rsn, _State)              -> ok.
code_change(_OldVsn, State, _Extra)  -> {ok, State}.

% ========= %
% Utilities %
% ========= %
respond({id, UserName, _VerKey, _IsOp}, #state{socket=Socket, name=Name, motd=MOTD, id=Id} = State)
	-> gen_tcp:send(Socket, protocol_lib:build({id, Name, MOTD, false}))
	,  gen_tcp:send(Socket, protocol_lib:build({lvl_init}))
	,  {DataPkts, {XSi, YSi, ZSi}, {XSp, YSp, ZSp, HSp, PSp}} = world_serv:data_pkts()
	,  [gen_tcp:send(Socket, DataPkt) || DataPkt <- DataPkts]
	,  gen_tcp:send(Socket, protocol_lib:build({lvl_fin, XSi, YSi, ZSi}))
	,  gen_tcp:send(Socket, protocol_lib:build({spawn, -1, UserName, {XSp, 0}, {YSp, 0}, {ZSp, 0}, HSp, PSp}))
	,  gen_tcp:send(Socket, protocol_lib:build({msg, 0, <<"&eWelcome to the server!">>}))
	,  Res = pg_call(
		{spawned
		, protocol_lib:build({spawn, Id, UserName, {XSp, 0}, {YSp, 0}, {ZSp, 0}, HSp, PSp})
		, protocol_lib:build({msg, 0, <<"&5", UserName/binary, " &ehas just joined!">>})
		}
	)
	,  [gen_tcp:send(protocol_lib:build({spawn, Id, UserName, {X, 0}, {Y, 0}, {Z, 0}, H, P})) || {Id, UserName, {X, Y, Z, H, P}} <- Res]
	,  State#state{username=UserName, pos={XSp, YSp, ZSp, HSp, PSp}}
	;
respond({set_block_m, X, Y, Z, _Mode, BlockType}, State)
	-> pg_cast(protocol_lib:build({set_block, X, Y, Z, BlockType}))
	,  State
	;
respond({pos_and_orient, _Id, {XInt, XFrac}, {YInt, YFrac}, {ZInt, ZFrac}, Heading, Pitch}, #state{id=Id} = State)
	-> pg_cast(protocol_lib:build({pos_and_orient, Id, {XInt, XFrac}, {YInt, YFrac}, {ZInt, ZFrac}, Heading, Pitch}))
	,  State#state{pos={XInt, YInt, ZInt, Heading, Pitch}}
	;
respond({msg, _Id, Msg}, #state{id=Id} = State)
	-> pg_cast(protocol_lib:build({msg, Id, Msg}))
	,  State
	;
respond(undefined, State) -> State.

pg_call(Msg) -> [gen_server:call(Pid, {pg, Msg}) || Pid <- lists:delete(self(), pg:get_members(updates))].
pg_cast(Msg) -> [gen_server:cast(Pid, {pg, Msg}) || Pid <- lists:delete(self(), pg:get_members(updates))].
