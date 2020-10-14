-module(bincomp).

-export([simple/0, bc/0, bc1/0, bsg/0, beam_asm1/2]).

simple() ->
    [Red || <<Red:2/binary, _Blue:2/binary>> <= <<1, 2, 3, 4, 5, 6, 7, 8>>].

bc() ->
    [1 - Bit || <<Bit:1>> <= <<240>>].

bc1() ->
    [<<(1 - Bit):1>> || <<Bit:1>> <= <<240>>].

bsg() ->
    << <<(1 - Bit):1>> || <<Bit:1>> <= <<240>> >>.

beam_asm1(<<"FunT", Keep:8/binary, Table0/binary>>, MD5) ->
    <<Uniq:27, _:101/bits>> = MD5,
    Table = finalize_fun_table_2(Table0, Uniq, <<>>),
    <<"FunT", Keep/binary, Table/binary>>.

finalize_fun_table_2(<<Keep:20/binary, 0:32, T/binary>>, Uniq, Acc) ->
    finalize_fun_table_2(T, Uniq, <<Acc/binary, Keep/binary, Uniq:32>>);
finalize_fun_table_2(<<>>, _, Acc) ->
    Acc.
