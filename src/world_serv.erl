-module(world_serv).
-behaviour(gen_server).
-export([start_link/0, data_pkts/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {arr, sizes, spawn}).
-define(SERVER, ?MODULE).

% === %
% API %
% === %
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
data_pkts()  -> gen_server:call(?SERVER, data_pkts).

% ========= %
% Callbacks %
% ========= %
init(_Args)
	-> process_flag(trap_exit, true)
	,  pg:join(updates, self())
	,  {ok, File} = file:read_file("priv/world.cw")
	,  GunzippedFile = zlib:gunzip(File)
	,  {ok, NBT} = erl_nbt:decode(GunzippedFile)
	,  #{"ClassicWorld" :=
		#{"X" := {short, XSi}
		, "Y" := {short, YSi}
		, "Z" := {short, ZSi}
		, "BlockArray" := {byte_array, BlockArr}
		, "Spawn" :=
			#{"X" := {short, XSp}
			, "Y" := {short, YSp}
			, "Z" := {short, ZSp}
			, "H" := {byte, HSp}
			, "P" := {byte, PSp}
			}
		}
	} = NBT
	,  {ok, #state{arr=BlockArr, sizes={XSi, YSi, ZSi}, spawn={XSp, YSp, ZSp, HSp, PSp}}}
	.

handle_call(data_pkts, _From, #state{arr=BlockArr, sizes=Sizes, spawn=Spawn} = State)
	-> BinArr = list_to_binary(BlockArr)
	,  Len = byte_size(BinArr)
	,  PrefixedArr = <<Len:32, BinArr/binary>>
	,  GzippedArr = zlib:gzip(PrefixedArr)
	,  {PaddingLen, Chunks} = chunksOf(1024, GzippedArr)
	,  ChunksCnt = length(Chunks)
	,  EnumChunks = lists:enumerate(Chunks)
	,  DataPkts = lists:map(
		fun({N, Chunk}) when N =:= ChunksCnt ->
			protocol_lib:build({lvl_data, 1024 - PaddingLen, Chunk, 100});
		   ({N, Chunk}) ->
			protocol_lib:build({lvl_data, 1024, Chunk, round(N / ChunksCnt * 100)})
		end,
		EnumChunks
	)
	,  {reply, {DataPkts, Sizes, Spawn}, State}
	;
handle_call(_Req, _From, State)     -> {reply, ok, State}.
handle_cast({pg, _Pkts}, State)     -> {noreply, State};
handle_cast(_Msg, State)            -> {noreply, State}.
handle_info(_Info, State)           -> {noreply, State}.
terminate(_Rsn, _State)             -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% ========= %
% Utilities %
% ========= %
chunksOf(Len, Bin)
	-> PaddingLen = case byte_size(Bin) rem Len of
		0 -> 0;
		N -> Len - N
	end
	,  Padding = binary:copy(<<16#00>>, PaddingLen)
	,  {PaddingLen, chunksOf(Len, <<Bin/binary, Padding/binary>>, [])}
	.
chunksOf(_Len, <<>>, Acc)
	-> lists:reverse(Acc)
	;
chunksOf(Len, List, Acc)
	-> {Head, Tail} = split_binary(List, Len)
	,  chunksOf(Len, Tail, [Head | Acc])
	.
