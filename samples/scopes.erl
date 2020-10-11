-module(scopes).

-export([noop/0, simple_scope/0, other_scope/1, multi_clause/1, case_match/0,
         receive_match/0, try_match/0, fun_scope/0, named_fun_scope/0,
         lc_scope/0, case_expr_not_matching/0]).

noop() ->
    ok.

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

case_match() ->
    A = 1,
    case make_ref() of
        A ->
            wat;
        B ->
            B = 1, % should match, defined in head of clause
            {ok, B}
    end.

receive_match() ->
    A = 1,
    receive
        A ->
            wat;
        B ->
            B = 1, % should match, defined in head of clause
            {ok, B}
    end.

try_match() ->
    A = 1,
    try
        make_ref()
    catch
        A ->
            ok;
        A:B ->
            B = 1, % should match, defined in head of clause
            {ok, B};
        C:B:A ->
            {ok, C, B}
    end.

fun_scope() ->
    A = 1,
    F =
        fun (B) ->
                B = 1, % match (from arg)
                A = 1, % match (from outer scope)
                C = B + A,
                C
        end,
    F(2),
    C = 3, % shouldn't match (vars in fun are in nested scope)
    B = 1, % shouldn't match (vars in fun are in nested scope)
    A + B + C.

named_fun_scope() ->
    A = 1,
    F =
        fun F1(B) ->
                B = 1, % match (from arg)
                A = 1, % match (from outer scope)
                C = B + A,
                C
        end,
    F(2),
    C = 3, % shouldn't match (vars in fun are in nested scope)
    B = 1, % shouldn't match (vars in fun are in nested scope)
    A + B + C.

lc_scope() ->
    O = 1,
    [begin
         O = 1, % match (from outer scope)
         case A of
             A ->
                 ok; % match (from generator)
             _ ->
                 wat
         end
     end
     || A <- lists:seq(0, 10)],
    A = 1, % shouldn't match (vars in list comprehension are in nested scope)
    A.

case_expr_not_matching() ->
    A = 1,
    case A % shouldn't match, it's not a pattern
        of
        1 ->
            ok
    end.
