-module(protocol).
-compile(export_all).

-type byte() :: 0 .. 255.
-type sbyte() :: -128 .. 127.
-type short() :: -32768 .. 32767.
-type nebin() :: nonempty_binary().
-type fbyte() :: nebin().
-type reason() :: distance | tile | clicking | lag.

%%%%%%%%%%%%%%%%%%%%
% Client -> Server %
%%%%%%%%%%%%%%%%%%%%
-spec read(nebin()) -> tuple().

read(<<16#00, 16#7, Name:64/binary, Key:64/binary, 16#0>>) ->
	{Name, Key};
read(<<16#05, X:16/signed, Y:16/signed, Z:16/signed, Mode, BlockType>>) ->
	{X, Y, Z, Mode, BlockType};
read(<<16#08, PlayerId/signed, X:8/binary-unit:2, Y:8/binary-unit:2, Z:8/binary-unit:2, Yaw:8, Pitch:8>>) ->
	{PlayerId, X, Y, Z, Yaw, Pitch};
read(<<16#0d, PlayerId/signed, Message:64/binary>>) ->
	{PlayerId, Message}.

%%%%%%%%%%%%%%%%%%%%
% Server -> Client %
%%%%%%%%%%%%%%%%%%%%
-spec serverId(string(), string(), boolean()) -> nebin().
-spec ping() -> nebin().
-spec levelInit() -> nebin().
-spec levelDataChunk(short(), nebin(), byte()) -> nebin().
-spec levelFinalise(short(), short(), short()) -> nebin().
-spec setBlock(short(), short(), short(), byte()) -> nebin().
-spec spawnPlayer(sbyte(), string(), fbyte(), fbyte(), fbyte(), byte(), byte()) -> nebin().
-spec setPosAndOrient(sbyte(), fbyte(), fbyte(), fbyte(), byte(), byte()) -> nebin().
-spec despawnPlayer(sbyte()) -> nebin().
-spec message(sbyte(), string()) -> nebin().
-spec disconnectPlayer(reason()) -> nebin().
-spec updateUserType(boolean()) -> nebin().

serverId(Name, MOTD, IsOp) ->
	PaddedName = list_to_binary(string:pad(Name, 64)),
	PaddedMOTD = list_to_binary(string:pad(MOTD, 64)),
	PlayerType = mkOp(IsOp),
	<<16#00, 16#07, PaddedName/binary, PaddedMOTD/binary, PlayerType>>.
ping() -> <<16#01>>.
levelInit() -> <<16#02>>.
levelDataChunk(Length, Data, PercentComplete) ->
	<<16#03, Length:16/signed, Data:1024/binary, PercentComplete:8>>.
levelFinalise(XSize, YSize, ZSize) ->
	<<16#04, XSize:16/signed, YSize:16/signed, ZSize:16/signed>>.
setBlock(X, Y, Z, BlockType) ->
	<<16#06, X:16/signed, Y:16/signed, Z:16/signed, BlockType:8>>.
spawnPlayer(PlayerId, Name, X, Y, Z, Yaw, Pitch) ->
	<<16#07, PlayerId/signed, X:8/binary-unit:2, Y:8/binary-unit:2,
	  Z:8/binary-unit:2, Yaw:8, Pitch:8>>.
setPosAndOrient(PlayerId, X, Y, Z, Yaw, Pitch) ->
	<<16#08, PlayerId:8/signed, X:8/binary-unit:2, Y:8/binary-unit:2,
	  Z:8/binary-unit:2, Yaw:8, Pitch:8>>.
despawnPlayer(PlayerId) ->
	<<16#0c, PlayerId:8/signed>>.
message(PlayerId, Message) ->
	PaddedMessage = list_to_binary(string:pad(Message, 64)),
	<<16#0d, PlayerId:8/signed, PaddedMessage/binary>>.
disconnectPlayer(Reason) ->
	StringReason = mkReason(Reason),
	PaddedReason = list_to_binary(string:pad(StringReason, 64)),
	<<16#0e, PaddedReason/binary>>.
updateUserType(IsOp) ->
	PlayerType = mkOp(IsOp),
	<<16#0f, PlayerType>>.

%%%%%%%%%%%%%
% Utilities %
%%%%%%%%%%%%%
-spec mkReason(reason()) -> string().
-spec mkOp(boolean()) -> char().

mkReason(distance) -> "Cheat detected: Distance";
mkReason(tile)     -> "Cheat detected: Tile type";
mkReason(clicking) -> "Cheat detected: Too much clicking!";
mkReason(lag)      -> "Cheat detected: Too much lag".

mkOp(true)  -> 16#64;
mkOp(false) -> 16#0.
