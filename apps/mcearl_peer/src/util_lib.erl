-module(util_lib).
-export([clamp/3, bin_pad/1, bin_trim_right/1, chunksOf/2]).

clamp(X, Min, Max) -> max(Min, min(X, Max)).

bin_pad(Bin)
	-> Padding = binary:copy(<<" ">>, 64 - size(Bin))
	,  <<Bin/binary, Padding/binary>>
	.
bin_trim_right(Bin)                 -> bin_rev(bin_trim_left(bin_rev(Bin))).
bin_trim_left(<<" ">>)              -> <<>>;
bin_trim_left(<<" ", Rest/binary>>) -> bin_trim_left(Rest);
bin_trim_left(Bin)                  -> Bin.
bin_rev(Bin)
	-> Size = erlang:bit_size(Bin)
	,  <<X:Size/integer-little>> = Bin
	,  <<X:Size/integer-big>>
	.

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
