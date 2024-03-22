-module(prop_protocol).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_id() ->
	?FORALL(Pkt, {id, str(), str(), boolean()},
		begin
			Pkt =:= protocol:parse(protocol:build(Pkt))
		end
	).

prop_pos_and_orient() ->
	?FORALL(Pkt, {pos_and_orient, sby(), fsh(), fsh(), fsh(), by(), by()},
		begin
			Pkt =:= protocol:parse(protocol:build(Pkt))
		end
	).

prop_msg() ->
	?FORALL(Pkt, {msg, sby(), str()},
		begin
			Pkt =:= protocol:parse(protocol:build(Pkt))
		end
	).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
biggest(List) -> foldl1(fun erlang:max/2, List).
foldl1(F, [X|Xs]) -> lists:foldl(F, X, Xs).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
by() -> range(0, 255).
sby() -> range(-128, 127).
sh() -> range(-32768, 32767).
fsh() -> {range(-1024, 1023), range(0, 31)}.
str() -> b().
byarr() -> binary(1024).


b() -> ?LET(Str, a(), string:trim(Str)).
a() -> ?SUCHTHAT(Str, list(ascii_char()), length(Str) =< 64).
ascii_char() -> ?SUCHTHAT(Ch, char(), Ch =< 127).
