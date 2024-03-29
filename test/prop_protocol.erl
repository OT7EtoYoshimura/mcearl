-module(prop_protocol).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_id() -> invertible({id, str(), str(), boolean()}).
prop_ping() -> invertible({ping}).
prop_lvl_init() -> invertible({lvl_init}).
prop_pos_and_orient() -> invertible({pos_and_orient, sby(), fsh(), fsh(), fsh(), by(), by()}).
prop_msg() -> invertible({msg, sby(), str()}).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
invertible(Data) ->
	proper:forall(
		Data,
		fun(Pkt) -> Pkt =:= protocol:parse(protocol:build(Pkt)) end
	).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
by() -> range(0, 255).
sby() -> range(-128, 127).
sh() -> range(-32768, 32767).
fsh() -> {range(-1024, 1023), range(0, 31)}.
str() -> ascii_binary().
byarr() -> binary(1024).

ascii_binary() -> ?LET(Bin, ascii_list_trimmed(), list_to_binary(Bin)).
ascii_list_trimmed() -> ?LET(Str, ascii_list(), string:trim(Str)).
ascii_list() -> ?SUCHTHAT(Str, list(ascii_char()), length(Str) =< 64).
ascii_char() -> ?SUCHTHAT(Ch, char(), Ch =< 127).
