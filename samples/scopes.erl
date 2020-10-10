-module(scopes).
-export([noop/0, simple_scope/0, other_scope/1, multi_clause/1]).

noop() -> ok.

simple_scope() ->
    A = 1,
    B = 2,
    A = 1,
    B = 2,
    A + B.

other_scope(A) ->
    A = 2,
    {A, 2} = {1, 2},
    B = A,
    A + B.

multi_clause(0) ->
    A = 1,
    {A, 2} = {1, 2},
    B = A,
    A + B;
multi_clause(1) ->
    A = 1,
    {A, 2} = {1, 2},
    B = A,
    A + B.
