-module(bincomp).

-export([simple/0, bc/0, bc1/0, bsg/0, beam_asm1/2, specifier/1, resolve_inst/4,
         byte_align/1, bin_literal_str/1, ascii_to_lower/1]).

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

specifier(Bs) ->
    Sz = bit_size(Bs),
    Unused = 8 - bit_size(Bs),
    <<Bs:Sz/bits, 0:Unused>>.

resolve_inst({bs_match_string = I, [F, Ms, {u, Bits}, {u, Off}]},
             _,
             Strings,
             _) ->
    Len = (Bits + 7) div 8,
    String =
        if Len > 0 ->
               <<_:Off/binary, Bin:Len/binary, _/binary>> = Strings,
               Bin;
           true ->
               <<>>
        end,
    {test, I, F, [Ms, Bits, String]}.

byte_align(Bs) ->
    case bit_size(Bs) rem 8 of
        0 ->
            size(Bs);
        N ->
            <<Bs/bitstring, 0:(8 - N)>>
    end.

bin_literal_str(<<"//"/utf8, Rest/binary>>) ->
    Rest.

ascii_to_lower(String) ->
    << <<(if $A =< C, C =< $Z ->
                  C + ($a - $A);
             true ->
                  C
          end)>>
       ||
        <<C>> <= iolist_to_binary(String) >>.
