-module(protocol_lib).
-export([parse_toc/1, parse/1, build/1]).

-define(STR_SIZE, 64).

% ============== %
% Packet Parsing %
% ============== %
parse_toc(Pkt) -> parse_toc(Pkt, []).
parse_toc(<<>>, Acc) -> Acc;
parse_toc(<<16#00, 16#7, Name:?STR_SIZE/binary, Key:?STR_SIZE/binary, IsOp, Rest/binary>>, Acc) ->
	parse_toc(Rest, [{id, bin_trim_right(Name), bin_trim_right(Key), toOp(IsOp)} | Acc]);
parse_toc(<<16#05, X:16/signed, Y:16/signed, Z:16/signed, Mode, BlockType, Rest/binary>>, Acc) ->
	parse_toc(Rest, [{set_block_m, X, Y, Z, toMode(Mode), BlockType} | Acc]);
parse_toc(<<16#08, PlayerId/signed, XInt:11/signed, XFrac:5, YInt:11/signed, YFrac:5, ZInt:11/signed, ZFrac:5, Heading, Pitch, Rest/binary>>, Acc) ->
	parse_toc(Rest, [{pos_and_orient, PlayerId, {XInt, XFrac}, {YInt, YFrac}, {ZInt, ZFrac}, Heading, Pitch} | Acc]);
parse_toc(<<16#0d, PlayerId/signed, Message:?STR_SIZE/binary, Rest/binary>>, Acc) ->
	parse_toc(Rest, [{msg, PlayerId, bin_trim_right(Message)} | Acc]);
parse_toc(Bin, Acc) -> [{undefined, Bin} | Acc].


-spec parse(binary()) -> tuple() | undefined.
parse(<<16#00, 16#7, Name:?STR_SIZE/binary, Key:?STR_SIZE/binary, IsOp>>)                                                                       -> {id, bin_trim_right(Name), bin_trim_right(Key), toOp(IsOp)};
parse(<<16#01>>)                                                                                                                                -> {ping};
parse(<<16#02>>)                                                                                                                                -> {lvl_init};
parse(<<16#03, Length:16/signed, Data:1024/binary, PercComp>>)                                                                                  -> {lvl_data, Length, Data, PercComp};
parse(<<16#04, XSize:16/signed, YSize:16/signed, ZSize:16/signed>>)                                                                             -> {lvl_fin, XSize, YSize, ZSize};
parse(<<16#05, X:16/signed, Y:16/signed, Z:16/signed, Mode, BlockType>>)                                                                        -> {set_block_m, X, Y, Z, toMode(Mode), BlockType};
parse(<<16#06, X:16/signed, Y:16/signed, Z:16/signed, BlockType>>)                                                                              -> {set_block, X, Y, Z, BlockType};
parse(<<16#07, PlayerId/signed, Name:?STR_SIZE/binary, XInt:11/signed, XFrac:5, YInt:11/signed, YFrac:5, ZInt:11/signed, ZFrac:5, Heading, Pitch>>) -> {spawn, PlayerId, bin_trim_right(Name), {XInt, XFrac}, {YInt, YFrac}, {ZInt, ZFrac}, Heading, Pitch};
parse(<<16#08, PlayerId/signed, XInt:11/signed, XFrac:5, YInt:11/signed, YFrac:5, ZInt:11/signed, ZFrac:5, Heading, Pitch>>)                        -> {pos_and_orient, PlayerId, {XInt, XFrac}, {YInt, YFrac}, {ZInt, ZFrac}, Heading, Pitch};
parse(<<16#09, PlayerId/signed, XInt:3/signed, XFrac:5, YInt:3/signed, YFrac:5, ZInt:3/signed, ZFrac:5, Heading, Pitch>>)                           -> {pos_and_orient_up, PlayerId, {XInt, XFrac}, {YInt, YFrac}, {ZInt, ZFrac}, Heading, Pitch};
parse(<<16#0a, PlayerId/signed, XInt:3/signed, XFrac:5, YInt:3/signed, YFrac:5, ZInt:3/signed, ZFrac:5>>)                                       -> {pos_up, PlayerId, {XInt, XFrac}, {YInt, YFrac}, {ZInt, ZFrac}};
parse(<<16#0b, PlayerId/signed, Heading, Pitch>>)                                                                                                   -> {orient_up, PlayerId, Heading, Pitch};
parse(<<16#0c, PlayerId/signed>>)                                                                                                               -> {despawn, PlayerId};
parse(<<16#0d, PlayerId/signed, Message:?STR_SIZE/binary>>)                                                                                     -> {msg, PlayerId, bin_trim_right(Message)};
parse(<<16#0e, Reason:?STR_SIZE/binary>>)                                                                                                       -> {disconnect, toReason(Reason)};
parse(<<16#0f, UserType>>)                                                                                                                      -> {user_type_up, toOp(UserType)};
parse(_)                                                                                                                                        -> undefined.

% =============== %
% Packet Building %
% =============== %
-spec build(tuple()) -> binary().
build({id, Name, MOTD, IsOp})
	-> PaddedName = bin_pad(Name)
	,  PaddedMOTD = bin_pad(MOTD)
	,  PlayerType = fromOp(IsOp)
	,  <<16#00, 16#07, PaddedName/binary, PaddedMOTD/binary, PlayerType>>
	;
build({ping})                                                                                 -> <<16#01>>;
build({lvl_init})                                                                             -> <<16#02>>;
build({lvl_data, Length, Data, PercComp})                                                     -> <<16#03, Length:16/signed, Data:1024/binary, PercComp>>;
build({lvl_fin, XSize, YSize, ZSize})                                                         -> <<16#04, XSize:16/signed, YSize:16/signed, ZSize:16/signed>>;
build({set_block_m, X, Y, Z, Mode, BlockType})
	-> ModeBin = fromMode(Mode)
	,  <<16#05, X:16/signed, Y:16/signed, Z:16/signed, ModeBin, BlockType>>
	;
build({set_block, X, Y, Z, BlockType})                                                        -> <<16#06, X:16/signed, Y:16/signed, Z:16/signed, BlockType>>;
build({spawn, PlayerId, Name, {XInt, XFrac}, {YInt, YFrac}, {ZInt, ZFrac}, Heading, Pitch})
	-> PaddedName = bin_pad(Name)
	,  <<16#07, PlayerId/signed, PaddedName/binary, XInt:11/signed, XFrac:5, YInt:11/signed, YFrac:5, ZInt:11/signed, ZFrac:5, Heading, Pitch>>
	;
build({pos_and_orient, PlayerId, {XInt, XFrac}, {YInt, YFrac}, {ZInt, ZFrac}, Heading, Pitch})    -> <<16#08, PlayerId/signed, XInt:11/signed, XFrac:5, YInt:11/signed, YFrac:5, ZInt:11/signed, ZFrac:5, Heading, Pitch>>;
build({pos_and_orient_up, PlayerId, {XInt, XFrac}, {YInt, YFrac}, {ZInt, ZFrac}, Heading, Pitch}) -> <<16#09, PlayerId/signed, XInt:3, XFrac:5, YInt:3, YFrac:5, ZInt:3, ZFrac:5, Heading, Pitch>>;
build({pos_up, PlayerId, {XInt, XFrac}, {YInt, YFrac}, {ZInt, ZFrac}})                        -> <<16#0a, PlayerId/signed, XInt:3, XFrac:5, YInt:3, YFrac:5, ZInt:3, ZFrac:5>>;
build({orient_up, PlayerId, Heading, Pitch})                                                      -> <<16#0b, PlayerId/signed, Heading, Pitch>>;
build({despawn, PlayerId})                                                                    -> <<16#0c, PlayerId/signed>>;
build({msg, PlayerId, Message})
	-> PaddedMessage = bin_pad(Message)
	,  <<16#0d, PlayerId/signed, PaddedMessage/binary>>
	;
build({disconnect, Reason})
	-> ReasonBin = fromReason(Reason)
	,  <<16#0e, ReasonBin/binary>>
	;
build({user_type_up, IsOp})
	-> PlayerType = fromOp(IsOp)
	,  <<16#0f, PlayerType>>
	.

% ========= %
% Utilities %
% ========= %
-type reason() :: distance | tile | clicking | lag.
-type mode()   :: created | destroyed.

-spec fromReason(reason() | undefined) -> binary().
-spec toReason(binary())               -> reason() | undefined.
-spec fromOp(boolean())                -> char().
-spec toOp(char())                     -> boolean().
-spec fromMode(mode() | undefined)     -> char().
-spec toMode(char())                   -> mode() | undefined.

-spec bin_pad(binary())        -> binary().
-spec bin_trim_right(binary()) -> binary().
-spec bin_trim_left(binary())  -> binary().
-spec bin_rev(binary())        -> binary().

fromReason(distance)  -> <<"Cheat detected: Distance                                        ">>;
fromReason(tile)      -> <<"Cheat detected: Tile type                                       ">>;
fromReason(clicking)  -> <<"Cheat detected: Too much clicking!                              ">>;
fromReason(lag)       -> <<"Cheat detected: Too much lag                                    ">>;
fromReason(undefined) -> <<"Cheat detected: undefined                                       ">>.

toReason(<<"Cheat detected: Distance                                        ">>) -> distance;
toReason(<<"Cheat detected: Tile type                                       ">>) -> tile;
toReason(<<"Cheat detected: Too much clicking!                              ">>) -> clicking;
toReason(<<"Cheat detected: Too much lag                                    ">>) -> lag;
toReason(_)                                                                      -> undefined.

fromOp(true)  -> 16#64;
fromOp(false) -> 16#00.

toOp(16#64) -> true;
toOp(16#00) -> false;
toOp(_)     -> false.

fromMode(created)   -> 16#01;
fromMode(destroyed) -> 16#00;
fromMode(_)         -> 16#00.

toMode(16#01) -> created;
toMode(16#00) -> destroyed;
toMode(_)     -> undefined.


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
