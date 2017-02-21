-module(ds_misc).

-export([to_hex/1, to_digit/1, bytes2int/2, bytes2int/4]).

to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    list_to_binary(to_hex(binary_to_list(Bin)));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].


to_digit(N) when N < 10 -> $0 + N;
to_digit(N) -> $a + N-10.

bytes2int(N1, N0) when 0 =< N1, N1 =< 255,
		       0 =< N0, N0 =< 255 ->
    (N1 bsl 8) bor N0.
bytes2int(N3, N2, N1, N0) when 0 =< N3, N3 =< 255,
			       0 =< N2, N2 =< 255,
			       0 =< N1, N1 =< 255,
			       0 =< N0, N0 =< 255 ->
    (N3 bsl 24) bor (N2 bsl 16) bor (N1 bsl 8) bor N0.


