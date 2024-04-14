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

handle_call({pg, {spawned, SpawnPkt, MsgPkt}}, From, #state{socket=Socket, id=Id, username=UserName, pos=Pos} = State)
	when Pos =/= undefined
	-> gen_server:reply(From, {Id, UserName, Pos})
	,  gen_tcp:send(Socket, SpawnPkt)
	,  gen_tcp:send(Socket, MsgPkt)
	,  logger:notice("PG Sent packets to: ~p~n~p~n~p~n", [Id, protocol_lib:parse(SpawnPkt), protocol_lib:parse(MsgPkt)])
	,  {noreply, State}
	;
handle_call(_Req, _From, State) -> {reply, ok, State}.

handle_cast({pg, Pkt}, #state{socket=Socket, id=Id} = State)
	-> gen_tcp:send(Socket, Pkt)
	,  logger:notice("PG Sent packet: ~p~nTo: ~p~n", [protocol_lib:parse(Pkt), Id])
	,  {noreply, State}
	;
handle_cast({accept, Socket, Pid, Id}, State)
	-> inet:setopts(Socket, [{active, once}, binary])
	,  {ok, {IP, _Port}} = inet:peername(Socket)
	,  timer:apply_interval(?MINUTE, gen_tcp, send, [Socket, protocol_lib:build({ping})])
	,  {noreply, State#state{socket=Socket, ip=IP, pid=Pid, id=Id}}
	;
handle_cast(_Msg, State) -> {noreply, State}.

handle_info({tcp, Socket, Msg}, #state{socket=Socket, id=Id} = State)
	-> NewState = lists:foldl(
		fun({undefined, Bin}, State)
			-> logger:notice("Could not parse packet:~n~p~nFrom: ~p~n", [Bin, Id])
			,  inet:setopts(Socket, [{active, once}])
			, State
		;  (Pkt, State)
			-> logger:notice("Received packet: ~p~nFrom: ~p~n", [Pkt, Id])
			,  inet:setopts(Socket, [{active, once}])
			,  respond(Pkt, State)
		end
		, State
		, protocol_lib:parse_toc(Msg)
	)
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
	-> send_packet(State, {id, Name, MOTD, false})
	,  send_packet(State, {lvl_init})
	,  {DataPkts, {XSi, YSi, ZSi}, {XSp, YSp, ZSp, HSp, PSp}} = world_serv:data_pkts()
	,  [gen_tcp:send(Socket, DataPkt) || DataPkt <- DataPkts]
	,  send_packet(State, {lvl_fin, XSi, YSi, ZSi})
	,  send_packet(State, {spawn, -1, UserName, {XSp, 0}, {YSp, 0}, {ZSp, 0}, HSp, PSp})
	,  send_packet(State, {msg, 0, <<"&eWelcome to the server!">>})
	,  Res = pg_call(
		{spawned
		, protocol_lib:build({spawn, Id, UserName, {XSp, 0}, {YSp, 0}, {ZSp, 0}, HSp, PSp})
		, protocol_lib:build({msg, 0, <<"&5", UserName/binary, " &ehas just joined!">>})
		}
	)
	,  [send_packet(State, {spawn, Id, UserName, {X, 0}, {Y, 0}, {Z, 0}, H, P}) || {Id, UserName, {X, Y, Z, H, P}} <- Res]
	,  State#state{username=UserName, pos={XSp, YSp, ZSp, HSp, PSp}}
	;
respond({set_block_m, X, Y, Z, _Mode, BlockType}, State)
	-> pg_cast(protocol_lib:build({set_block, X, Y, Z, BlockType}))
	,  State
	;
respond({pos_and_orient, -1, {XInt, XFrac}, {YInt, YFrac}, {ZInt, ZFrac}, Heading, Pitch}, #state{id=Id} = State)
	-> pg_cast(protocol_lib:build({pos_and_orient, Id, {XInt, XFrac}, {YInt, YFrac}, {ZInt, ZFrac}, Heading, Pitch}))
	,  State#state{pos={XInt, YInt, ZInt, Heading, Pitch}}
	;
respond({msg, -1, <<"/", Rest/binary>>}, State) -> command(binary:split(Rest, <<" ">>, [global, trim_all]), State);
respond({msg, -1, Msg}, #state{socket=Socket, id=Id, username=UserName} = State)
	-> message(Msg, State)
	,  State
	;
respond(undefined, State) -> State.

message(Msg, #state{username=UserName} = State)
	when byte_size(Msg) < (60 - byte_size(UserName))
	-> send_msg(Msg, State)
	;
message(Msg, #state{username=UserName} = State)
	-> BitOffset  = 60 - byte_size(UserName)
	,  ByteOffset = BitOffset - (BitOffset rem 8)
	, {First, Second} = split_binary(Msg, ByteOffset)
	, send_msg(First, State)
	, send_msg(Second, State)
	.
send_msg(Msg, #state{id=Id, username=UserName} = State)
	-> PrependedMessage = <<"<", UserName/binary, ">: ", Msg/binary>>
	,  pg_cast(protocol_lib:build({msg, Id, PrependedMessage}))
	,  send_packet(State, {msg, -1, PrependedMessage})
	.

command([<<"tp">>, XBin, YBin, ZBin], #state{id=Id, pos={_,_,_,H,P}} = State)
	-> X = util_lib:clamp(binary_to_integer(XBin), -1024, 1023)
	,  Y = util_lib:clamp(binary_to_integer(YBin), -1024, 1024)
	,  Z = util_lib:clamp(binary_to_integer(ZBin), -1024, 1023)
	,  pg_cast(protocol_lib:build({pos_and_orient, Id, {X, 0}, {Y, 0}, {Z,0}, H, P}))
	,  send_packet(State, {pos_and_orient, -1, {X, 0}, {Y, 0}, {Z, 0}, H, P})
	,  State#state{pos={X, Y, Z, H, P}}
	;
command(_, State)
	-> send_packet(State, {msg, 0, <<"&cUnknown command.">>})
	,  State
	.

pg_call(Msg) -> [gen_server:call(Pid, {pg, Msg}) || Pid <- lists:delete(self(), pg:get_members(updates))].
pg_cast(Msg) -> [gen_server:cast(Pid, {pg, Msg}) || Pid <- lists:delete(self(), pg:get_members(updates))].

send_packet(#state{socket=Socket, id=Id}, PktTuple)
	-> gen_tcp:send(Socket, protocol_lib:build(PktTuple))
	,  logger:notice("Sent packet: ~p~nTo: ~p~n", [PktTuple, Id])
	.

