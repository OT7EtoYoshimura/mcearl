-module(world).
-export([read/1]).

read(FileName) ->
	{ok, File} = file:read_file("priv/" ++ FileName),
	GunzippedFile = zlib:gunzip(File),
	{ok, NBT} = erl_nbt:decode(GunzippedFile),
	#{"ClassicWorld" :=
		(ClassicWorld =
			#{"X" := {short, X},
			  "Y" := {short, Y},
			  "Z" := {short, Z},
			  "BlockArray" := {byte_array, BlockArr}
			}
		)
	} = NBT,
	{PaddingLen, Chunks} = prepare(BlockArr),
	DataPkts = data_pkts(PaddingLen, Chunks),
	{DataPkts, X, Y, Z}.

% Before being sent to the client:
%	- it is prefixed with its length as a big-endian int (4 bytes)
%	- gzipped (not zlib or plain deflate)
%	- this is then split into 1024 byte chunks, to be sent to the client
%	- the final chunk is padded with 0x00 bytes.
prepare(BlockArr) ->
	BinArr = list_to_binary(BlockArr),
	Len = byte_size(BinArr),
	PrefixedArr = <<Len:32, BinArr/binary>>,
	GzippedArr = zlib:gzip(PrefixedArr),
	chunksOf(1024, GzippedArr).

chunksOf(Len, Bin) ->
	PaddingLen = case byte_size(Bin) rem Len of
		0 -> 0;
		N -> Len - N
	end,
	Padding = binary:copy(<<16#00>>, PaddingLen),
	{PaddingLen, chunksOf(Len, <<Bin/binary, Padding/binary>>, [])}.
chunksOf(Len, <<>>, Acc) ->
	lists:reverse(Acc);
chunksOf(Len, List, Acc) ->
	{Head, Tail} = split_binary(List, Len),
	chunksOf(Len, Tail, [Head | Acc]).

data_pkts(PaddingLen, Chunks) ->
	ChunksCnt = length(Chunks),
	EnumChunks = lists:enumerate(Chunks),
	lists:map(
		fun({N, Chunk}) when N =:= ChunksCnt ->
			protocol:build({lvl_data, 1024 - PaddingLen, Chunk, 100});
		   ({N, Chunk}) ->
			protocol:build({lvl_data, 1024, Chunk, round(N / ChunksCnt * 100)})
		end,
		EnumChunks
	 ).
