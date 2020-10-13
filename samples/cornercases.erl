-module(cornercases).

-export([varnames/12, atoms/0, chars/0, calls/3, calls/0, long_calc/0]).

varnames(When, And, Or, Not, In, Fn, Do, End, Catch, Rescue, After, Else) ->
    {When, And, Or, Not, In, Fn, Do, End, Catch, Rescue, After, Else}.

atoms() ->
    ['type-id'].

chars() ->
    [$\s, $\t, $\r, $\n, $\f, $\e, $\d, $\b, $\v].

calls(M, F, Arity) ->
    {F(),
     fun calls/3,
     fun cornercases:calls/3,
     fun M:F/Arity,
     fun M:calls/3,
     fun M:F/3,
     fun cornercases:F/Arity,
     fun cornercases:calls/Arity,
     fun M:calls/Arity}.

calls() ->
    M = erlang,
    F = max,
    {M:max(1, 2), M:F(1, 2), erlang:F(1, 2), erlang:max(1, 2), max(1, 2)}.

long_calc() ->
    1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 +
        1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1
        + 1.
