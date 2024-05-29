-module(server).
-behaviour(gen_server).
-export([start_link/2, proxy/2, intro/5]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {name, motd, ip, proxy, controller, id, username, pos}).
-define(MINUTE, 60*1000).

% === %
% API %
% === %
start_link(Name, MOTD)    -> gen_server:start_link(?MODULE, [Name, MOTD], []).
proxy(Pid, Data)          -> gen_server:cast(Pid, {proxy, Data}).
intro(Pid, Id, Ip, Proxy, Controller) -> gen_server:cast(Pid, {intro, Id, Ip, Proxy, Controller}).

% ========= %
% Callbacks %
% ========= %
init([Name, MOTD])
	-> pg:join(node(), servers, self())
	,  gen_server:cast({global, heartbeat}, alive)
	,  {ok, #state{name=list_to_binary(Name), motd=list_to_binary(MOTD)}}
	.

handle_call({pg, {spawned, SpawnPkt, MsgPkt}}, From, #state{id=Id, username=UserName, pos=Pos} = State)
	when Pos =/= undefined
	-> gen_server:reply(From, {Id, UserName, Pos})
	,  send_packet(State, SpawnPkt)
	,  send_packet(State, MsgPkt)
	,  {noreply, State}
	;
handle_call(_Req, _From, State)     -> {reply, ok, State}.

handle_cast({proxy, Data}, State)
	-> NewState = lists:foldl
		(fun(Pkt, State) -> respond(Pkt, State) end
		, State
		, protocol_lib:parse_toc(Data)
		)
	, {noreply, NewState}
	;
handle_cast({pg, Pkt}, State)
	-> send_packet(State, Pkt)
	,  {noreply, State}
	;
handle_cast(ping, State)
	-> send_packet(State, {ping})
	,  {noreply, State}
	;
handle_cast({intro, Id, Ip, Proxy, Controller}, State)
	-> timer:apply_repeatedly(?MINUTE, gen_server, cast, [self(), ping])
	,  {noreply, State#state{id=Id, ip=Ip, proxy=Proxy, controller=Controller}}
	;
handle_cast(_Msg, State)            -> {noreply, State}.

handle_info(_Info, State)           -> {noreply, State}.
terminate(_Rsn, _State)             -> gen_server:cast({global, heartbeat}, dead).
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% ========= %
% Utilities %
% ========= %
respond({id, UserName, _VerKey, _IsOp}, #state{name=Name, motd=MOTD, id=Id, proxy=Proxy} = State)
	-> send_packet(State, {id, Name, MOTD, false})
	,  send_packet(State, {lvl_init})
	,  {DataPkts, {XSi, YSi, ZSi}, {XSp, YSp, ZSp, HSp, PSp}} = gen_server:call({global, world}, data_pkts)
	,  [send_packet(State, DataPkt) || DataPkt <- DataPkts]
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
respond({msg, -1, Msg}, #state{id=Id, username=UserName} = State)
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


send_packet(#state{proxy=Proxy}, Pkt)
	when is_binary(Pkt)
	-> gen_server:cast(Proxy, {reply, Pkt})
	;
send_packet(#state{proxy=Proxy}, Pkt)
	when is_tuple(Pkt)
	-> gen_server:cast(Proxy, {reply, protocol_lib:build(Pkt)})
	.

pg_call(Msg) -> [gen_server:call(Pid, {pg, Msg}) || Pid <- lists:delete(self(), pg:get_members(servers))].
pg_cast(Msg) -> [gen_server:cast(Pid, {pg, Msg}) || Pid <- lists:delete(self(), pg:get_members(servers))].
