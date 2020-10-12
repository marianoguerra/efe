-module(boollogic).

-export([o_and/2, o_or/2, o_xor/2]).

o_and(A, B) ->
    A and B.

o_or(A, B) ->
    A or B.

o_xor(A, B) ->
    A xor B.
