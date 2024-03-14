-module(protocol).
-compile(export_all).

-spec mkReason(distance | tile | clicking | lag) -> string().

%%%%%%%%%%%%%%%%%%%%
% Client -> Server %
%%%%%%%%%%%%%%%%%%%%
read(<<16#00, 16#7, Name:64/binary, Key:64/binary, 16#0>>) -> 16#00;
read(<<16#05, X:16/signed, Y:16/signed, Z:16/signed,
       Mode, BlockType>>) -> 16#05;
read(<<16#08, PlayerId/signed, X:8/binary-unit:2, Y:8/binary-unit:2,
       Z:8/binary-unit:2, Yaw:8, Pitch:8>>) -> 16#08;
read(<<16#0d, PlayerId/signed, Message:64/binary>>) -> 16#0d.

%%%%%%%%%%%%%%%%%%%%
% Server -> Client %
%%%%%%%%%%%%%%%%%%%%
serverId(Name, MOTD, IsOp) ->
	PaddedName = list_to_binary(string:pad(Name, 64)),
	PaddedMOTD = list_to_binary(string:pad(MOTD, 64)),
	PlayerType = if IsOp -> 16#64; true -> 16#00 end,
	<<16#00, 16#07, PaddedName/binary, PaddedMOTD/binary, PlayerType>>.
ping() -> <<16#01>>.
levelInit() -> <<16#02>>.
levelDataChunk(Length, Data, PercentComplete) ->
	<<16#03, Length:16/signed, Data:1024/binary, PercentComplete>>.
levelFinalise(XSize, YSize, ZSize) ->
	<<16#04, XSize:16/signed, YSize:16/signed, ZSize:16/signed>>.
setBlock(X, Y, Z, BlockType) ->
	<<16#06, X:16/signed, Y:16/signed, Z:16/signed, BlockType:8>>.
spawnPlayer(Id, Name, X, Y, Z, Yaw, Pitch) ->
	<<16#07, Id/signed, X:8/binary-unit:2, Y:8/binary-unit:2,
	  Z:8/binary-unit:2, Yaw:8, Pitch:8>>.
setPosAndOrient(Id, X, Y, Z, Yaw, Pitch) ->
	<<16#08, Id:8/signed, X:8/binary-unit:2, Y:8/binary-unit:2,
	  Z:8/binary-unit:2, Yaw:8, Pitch:8>>.
despawnPlayer(Id) -> <<16#0c, Id:8/signed>>.
message(Id, Message) ->
	PaddedMessage = list_to_binary(string:pad(Message, 64)),
	<<16#0d, Id:8/signed, PaddedMessage/binary>>.
disconnectPlayer(Reason) ->
	StringReason = mkReason(Reason),
	PaddedReason = list_to_binary(string:pad(StringReason, 64)),
	<<16#0e, PaddedReason/binary>>.
updatedUserType(IsOp) -> <<16#0f, if IsOp -> 16#64; true -> 16#00 end>>.

%%%%%%%%%%%
% Helpers %
%%%%%%%%%%%
mkReason(distance) -> "Cheat detected: Distance";
mkReason(tile)     -> "Cheat detected: Tile type";
mkReason(clicking) -> "Cheat detected: Too much clicking!";
mkReason(lag)      -> "Cheat detected: Too much lag".
