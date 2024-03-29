-module(server).
-export[start/0, run/1].

-define(MAXUSR, "32").
-define(NAME, "Testing....").
-define(MOTD, "Testing my server implementation; not playable !").

start() ->
	{ok, Listen} = gen_tcp:listen(8091, [{active, true}, binary]),
	heartbeat:apply_interval(?MAXUSR, ?NAME),
	run(Listen).

run(Listen) ->
	{ok, Accept} = gen_tcp:accept(Listen),
	receive {tcp, Accept, Msg} ->
		case protocol:parse(Msg) of
			{id, Name, Key, _IsOp} ->
				welcome(Accept),
				run(Listen);
			{set_block_m, X, Y, Z, Mode, 16#00} ->
				run(Listen);
			{pos_and_orient, -1, X, Y, Z, Yaw, Pitch} ->
				run(Listen);
			{msg, -1, Message} ->
				run(Listen)
		end
	end.

welcome(Accept) ->
	gen_tcp:send(Accept, protocol:build({id, ?NAME, ?MOTD, false})),
	gen_tcp:send(Accept, protocol:build({lvl_init})),
	{PaddingLen, Chunks, X, Y, Z} = world:read("world.cw"),
	LvlDataPkts = build_lvl_data_pkts(PaddingLen, Chunks),
	lists:foreach(fun(Pkt) -> gen_tcp:send(Accept, Pkt) end, LvlDataPkts),
	gen_tcp:send(Accept, protocol:build({lvl_fin, X, Y, Z})).

build_lvl_data_pkts(PaddingLen, Chunks) ->
	ChunksCnt = length(Chunks),
	EnumChunks = lists:enumerate(Chunks),
	lists:map(
		fun({N, Chunk}) when N =:= ChunksCnt ->
			protocol:build({lvl_data}, 1024 - PaddingLen, Chunk, 100);
		   ({N, Chunk}) ->
			protocol:build({lvl_data}, 1024, Chunk, N / ChunksCnt * 100)
		end,
		EnumChunks
	).
