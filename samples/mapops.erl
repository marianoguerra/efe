-module(mapops).
-export([put_atom/0, put_key/0, quoted_atom_key/1]).

put_atom() ->
    M = #{},
    M0 = M#{},
    M1 = M#{a => 1},
    M2 = M1#{a := 1},
    M3 = M#{a => 1, b => 2},
    M4 = M3#{a := 1, b => 2},
    M5 = M1#{a := 1, b := 2},
    M6 = M1#{a := 1, b := 2, c => 3, d => 4},
    {M0, M1, M2, M3, M4, M5, M6}.

put_key() ->
    M = #{},
    M1 = M#{<<"a">> => 1},
    M2 = M1#{<<"a">> := 1},
    M3 = M#{<<"a">> => 1, <<"b">> => 2},
    M4 = M3#{<<"a">> := 1, <<"b">> => 2},
    M5 = M1#{<<"a">> := 1, <<"b">> := 2},
    M6 = M1#{<<"a">> := 1, <<"b">> := 2, <<"c">> => 3, <<"d">> => 4},
    {M1, M2, M3, M4, M5, M6}.

quoted_atom_key(M) ->
    M#{'a-b' := 1}.
