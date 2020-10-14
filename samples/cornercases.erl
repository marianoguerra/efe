-module(cornercases).

-export([varnames/12, atoms/0, chars/0, calls/3, calls/0, long_calc/0, binops/0,
         instant_call/0, lc_no_gen/0, module_macro/0, uppercase_funs/2]).

varnames(When, And, Or, Not, In, Fn, Do, End, Catch, Rescue, After, Else) ->
    {When, And, Or, Not, In, Fn, Do, End, Catch, Rescue, After, Else}.

atoms() ->
    ['type-id'].

chars() ->
    [$\s, $\t, $\r, $\n, $\f, $\e, $\d, $\b, $\v, $\^G, $\^C].

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

binops() ->
    bnot (1 bsl 2).

instant_call() ->
    fun () ->
            ok
    end().

lc_no_gen() ->
    % elixir for needs a generator, add a dummy `_ <- [nil]` generator
    {[ok || true],
     [ok || false],
     [ok || _ <- [nil], true],
     [ok || _ <- [nil], false]}.

module_macro() ->
    ?MODULE.

uppercase_funs(A, B) ->
    wxMenu:'Destroy'(A, B).
