-module(prop_protocol).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_id() -> invertible({id, str(), str(), boolean()}).
prop_ping() -> invertible({ping}).
prop_lvl_init() -> invertible({lvl_init}).
prop_lvl_data() -> invertible({lvl_data, sh(), byarr(), by()}).
prop_lvl_fin() -> invertible({lvl_fin, sh(), sh(), sh()}).
prop_set_block_m() -> invertible({set_block_m, sh(), sh(), sh(), mode(), by()}).
prop_set_block() -> invertible({set_block, sh(), sh(), sh(), by()}).
prop_spawn() -> invertible({spawn, sby(), str(), fsh(), fsh(), fsh(), by(), by()}).
prop_pos_and_orient() -> invertible({pos_and_orient, sby(), fsh(), fsh(), fsh(), by(), by()}).
prop_pos_and_orient_up() -> invertible({pos_and_orient_up, sby(), fby(), fby(), fby(), by(), by()}).
prop_pos_up() -> invertible({pos_up, sby(), fby(), fby(), fby()}).
prop_orient_up() -> invertible({orient_up, sby(), by(), by()}).
prop_despawn() -> invertible({despawn, sby()}).
prop_msg() -> invertible({msg, sby(), str()}).
prop_disconnect() -> invertible({disconnect, rsn()}).
prop_user_type_up() -> invertible({user_type_up, boolean()}).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
invertible(Data) ->
	proper:forall(
		Data,
		fun(Pkt) -> Pkt =:= protocol_lib:parse(protocol_lib:build(Pkt)) end
	).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
by() -> range(0, 255).
sby() -> range(-128, 127).
fby() -> {range(-4, 3), range(0, 31)}.
sh() -> range(-32768, 32767).
fsh() -> {range(-1024, 1023), range(0, 31)}.
str() -> ascii_binary().
byarr() -> binary(1024).
mode() -> oneof([created, destroyed]).
rsn() -> oneof([distance, tile, clicking, lag]).

ascii_binary() -> ?LET(Bin, ascii_list_trimmed(), list_to_binary(Bin)).
ascii_list_trimmed() -> ?LET(Str, ascii_list(), string:trim(Str)).
ascii_list() -> ?SUCHTHAT(Str, list(ascii_char()), length(Str) =< 64).
ascii_char() -> ?SUCHTHAT(Ch, char(), Ch =< 127).
