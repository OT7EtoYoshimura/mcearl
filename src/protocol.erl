-module(protocol).
-export([parse/1, build/1]).

-define(STR_SIZE, 64).
-define(WS, 32).
%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Protocol Specific types %
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -type types() :: by() | sby() | fby() | sh() | fsh() | str() | byarr().
% -type pkt_id()   :: id | set_block | pos_and_orient | msg | ping | lvl_init
%                   | lvl_data | lvl_fin | spawn | pos_and_orient_up | pos_up
%                   | orient_up | despawn | disconnect | user_type_up.

-type by()    :: 0 .. 255.
-type sby()   :: -128 .. 127.
% -type fby()   :: {-4..3, 0..31}.
-type sh()    :: -32768 .. 32767.
-type fsh()   :: {-1024..1023, 0..31}.
-type str()   :: string().
-type byarr() :: binary().

-type pkt_load() :: {id, str(), str(), boolean()}
                  | {set_block,   sh(), sh(), sh(),         by()}
                  | {set_block_m, sh(), sh(), sh(), mode(), by()}
                  | {pos_and_orient, sby(), fsh(), fsh(), fsh(), by(), by()}
                  | {msg, sby(), str()}
                  | {ping} | {lvl_init}
                  | {lvl_data, sh(), byarr(), by()}
                  | {lvl_fin, sh(), sh(), sh()}
                  | {spawn, sby(), str(), fsh(), fsh(), fsh(), by(), by()}
                  | {despawn, sby()}
                  | {disconnect, reason()}
                  | {user_type_up, boolean()}.

%%%%%%%%%%%%%%%%%%%%
% Client -> Server %
%%%%%%%%%%%%%%%%%%%%
-spec parse(binary()) -> pkt_load() | undefined.

parse(<<16#00, 16#7, Name:?STR_SIZE/binary, Key:?STR_SIZE/binary, IsOp>>) ->
	TrimmedName = trim(Name),
	TrimmedKey  = trim(Key),
	{id, TrimmedName, TrimmedKey, toOp(IsOp)}; % IsOp is allways 16#00
parse(<<16#05, X:16/signed, Y:16/signed, Z:16/signed, Mode, BlockType>>) ->
	{set_block_m, mkSh(X), mkSh(Y), mkSh(Z), toMode(Mode), mkBy(BlockType)};
parse(<<16#08, PlayerId/signed, XInt:11/signed, XFrac:5, YInt:11/signed, YFrac:5, ZInt:11/signed, ZFrac:5, Yaw, Pitch>>) ->
	{pos_and_orient, mkSby(PlayerId), mkFsh(XInt, XFrac), mkFsh(YInt, YFrac), mkFsh(ZInt, ZFrac), mkBy(Yaw), mkBy(Pitch)};
parse(<<16#0d, PlayerId/signed, Message:?STR_SIZE/binary>>) ->
	TrimmedMessage = string:trim(binary_to_list(Message)),
	{msg, mkSby(PlayerId), TrimmedMessage};
parse(_) -> undefined.

%%%%%%%%%%%%%%%%%%%%
% Server -> Client %
%%%%%%%%%%%%%%%%%%%%
-spec build(pkt_load()) -> binary() | undefined.

build({id, Name, MOTD, IsOp}) ->
	PaddedName = pad(Name),
	PaddedMOTD = pad(MOTD),
	PlayerType = fromOp(IsOp),
	<<16#00, 16#07, PaddedName/binary, PaddedMOTD/binary, PlayerType>>;
build({ping}) -> <<16#01>>;
build({lvl_init}) -> <<16#02>>;
build({lvl_data, Length, Data, PercComp}) ->
	<<16#03, Length:16/signed, Data:1024/binary, PercComp>>;
build({lvl_fin, XSize, YSize, ZSize}) ->
	<<16#04, XSize:16/signed, YSize:16/signed, ZSize:16/signed>>;
build({set_block, X, Y, Z, BlockType}) ->
	<<16#06, X:16/signed, Y:16/signed, Z:16/signed, BlockType>>;
build({spawn, PlayerId, Name, {XInt, XFrac}, {YInt, YFrac}, {ZInt, ZFrac}, Yaw, Pitch}) ->
	PaddedName = pad(Name),
	<<16#07, PlayerId/signed, PaddedName/binary, XInt:11/signed, XFrac:5, YInt:11/signed, YFrac:5, ZInt:11/signed, ZFrac:5, Yaw, Pitch>>;
build({pos_and_orient, PlayerId, {XInt, XFrac}, {YInt, YFrac}, {ZInt, ZFrac}, Yaw, Pitch}) ->
	<<16#08, PlayerId/signed, XInt:11/signed, XFrac:5, YInt:11/signed, YFrac:5, ZInt:11/signed, ZFrac:5, Yaw, Pitch>>;
build({despawn, PlayerId}) ->
	<<16#0c, PlayerId/signed>>;
build({msg, PlayerId, Message}) ->
	PaddedMessage = pad(Message),
	<<16#0d, PlayerId/signed, PaddedMessage/binary>>;
build({disconnect, Reason}) ->
	StringReason = fromReason(Reason),
	PaddedReason = pad(StringReason),
	<<16#0e, PaddedReason/binary>>;
build({user_type_up, IsOp}) ->
	PlayerType = fromOp(IsOp),
	<<16#0f, PlayerType>>;
build(_) -> undefined.

%%%%%%%%%%%%%
% Utilities %
%%%%%%%%%%%%%
-type reason() :: distance | tile | clicking | lag.
-type mode()   :: created | destroyed.

-spec fromReason(reason()) -> string().
-spec fromOp(boolean()) -> 16#64 | 16#00.
-spec toOp(16#64 | 16#00) -> boolean().
-spec mkSh(integer()) -> sh().
-spec mkBy(integer()) -> by().
-spec mkSby(integer()) -> sby().
% -spec mkFby(integer(), integer()) -> fby().
-spec mkFsh(integer(), integer()) -> fsh().
-spec trim(binary()) -> str().
-spec pad(str()) -> binary().
-spec clamp(integer(), integer(), integer()) -> integer().

fromReason(distance) -> "Cheat detected: Distance";
fromReason(tile)     -> "Cheat detected: Tile type";
fromReason(clicking) -> "Cheat detected: Too much clicking!";
fromReason(lag)      -> "Cheat detected: Too much lag".

fromOp(true)  -> 16#64;
fromOp(false) -> 16#00.

toOp(16#64) -> true;
toOp(16#00) -> false.

% fromMode(created)   -> 16#01;
% fromMode(destroyed) -> 16#00.

toMode(16#01) -> created;
toMode(16#00) -> destroyed;
toMode(_)     -> undefined.

mkSh(X) -> clamp(X, -32768, 32767).
mkBy(X) -> clamp(X, 0, 255).
mkSby(X) -> clamp(X, -128, 127).
% mkFby(Integral, Frac) -> {clamp(Integral,    -4,   3), clamp(Frac, 0, 31)}.
mkFsh(Integral, Frac) -> {clamp(Integral, -1024,1023), clamp(Frac, 0, 31)}.

trim(Bin) ->
	Str = binary_to_list(Bin),
	Rev = lists:reverse(Str),
	Trimmed = lists:dropwhile(fun(Ch) -> Ch =:= ?WS end, Rev),
	lists:reverse(Trimmed).
pad(Str) ->
	Len = length(Str),
	Padding = lists:duplicate(?STR_SIZE - Len, " "),
	Padded = Str ++ Padding,
	list_to_binary(Padded).

clamp(X, Min, Max) -> min(max(X, Min), Max).
