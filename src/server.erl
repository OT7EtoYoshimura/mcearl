-module(server).
-export[start/0, chunksOf/2, world/0].

-define(MAXUSR, "32").
-define(NAME, "Testing....").
-define(MOTD, "Testing my server implementation; not playable !").

start() ->
	heartbeat:apply_interval(?MAXUSR, ?NAME),
	run().

run() ->
	{ok, Listen} = gen_tcp:listen(8091, [{active, true}, binary]),
	{ok, Accept} = gen_tcp:accept(Listen),
	receive {tcp, Accept, Msg} ->
		case protocol:parse(Msg) of
			{id, Name, Key, _IsOp} ->
				gen_tcp:send(Accept, protocol:build({id, ?NAME, ?MOTD, false})),
				gen_tcp:send(Accept, protocol:build({lvl_init})),
				gen_tcp:send(Accept, protocol:build({lvl_data})),
				{ByteArray, X, Y, Z} = world(),
				gen_tcp:send(Accept, protocol:build({lvl_fin, X, Y, Z})),
				run();
			{set_block_m, X, Y, Z, Mode, 16#00} ->
				run();
			{pos_and_orient, -1, X, Y, Z, Yaw, Pitch} ->
				run();
			{msg, -1, Message} ->
				run()
		end
	end.

world() ->
	{ok, File} = file:read_file("priv/world.cw"),
	GunzippedFile = zlib:gunzip(File),
	{ok, NBT} = erl_nbt:decode(GunzippedFile),
	#{"ClassicWorld" := ClassicWorld} = NBT,
	#{ "X" := {short, X}, "Y" := {short, Y}, "Z" := {short, Z}} = ClassicWorld,
	#{"BlockArray" := {byte_array, ByteArray}} = ClassicWorld,
	Len = length(ByteArray),
	Blocks = list_to_binary(ByteArray),
	Prefixed = <<Len:32, Blocks/binary>>,
	Gzipped = zlib:gzip(Prefixed),
	chunksOf(1024, 
	{ByteArray, X, Y, Z}.

chunksOf(Len, Bin) ->
	LeaderLen = case byte_size(Bin) rem Len of
		0 -> 0;
		N -> Len - N
	end,
	Leader = binary:copy(<<16#00>>, LeaderLen),
        chunksOf(Len, <<Bin/binary, Leader/binary>>, []).
chunksOf(Len, <<>>, Acc) ->
	lists:reverse(Acc);
chunksOf(Len, List, Acc) ->
	{Head, Tail} = split_binary(List, Len),
	chunksOf(Len, Tail, [Head | Acc]).
