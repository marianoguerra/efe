-module(scopes).

-export([noop/0, simple_scope/0, other_scope/1, multi_clause/1, case_match/0,
         receive_match/0, try_match/0, fun_scope/0, named_fun_scope/0,
         lc_scope/0, case_expr_not_matching/0, vars_in_clauses/1,
         var_in_prev_fun/1, var_in_prev_fun1/1, match_in_head/2, if_clauses/1,
         try_stacktrace/1, new_var_in_match/1, map_key_match/1,
         right_head_match/2, match_on_right_side/0, lc_pattern_vars_in_expr/0,
         match_left_var_not_on_right_side/0, take/2]).

-record(user, {username = <<"meg">>}).
-record(handler,
        {module :: atom(),
         id = false,
         state,
         supervised = false :: false | pid()}).

-callback format_status(Opt, StatusData) -> Status when Opt :: normal |
                                                               terminate,
                                                        StatusData :: [PDict |
                                                                       State],
                                                        PDict :: [{Key ::
                                                                       term(),
                                                                   Value ::
                                                                       term()}],
                                                        State :: term(),
                                                        Status :: term().

-type gb_tree_node(K, V) :: nil |
                            {K, V, gb_tree_node(K, V), gb_tree_node(K, V)}.

-opaque tree(Key, Value) :: {non_neg_integer(), gb_tree_node(Key, Value)}.

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

vars_in_clauses(A) ->
    case A of
        {B, 1} ->
            B;
        {1, B} ->
            B
    end.

var_in_prev_fun(A) ->
    A.

var_in_prev_fun1(B) ->
    A = B + B - B + -B,
    A.

match_in_head(Username, #user{username = Username}) ->
    Username.

if_clauses(A) ->
    if A ->
           B = 1,
           B + 1;
       true ->
           B = 2, % shouldn't match
           B + 2
    end.

try_stacktrace(F) ->
    try
        F()
    catch
        T:(E = asd):S ->
            {T, E, S};
        E ->
            E;
        T:E ->
            {T, E}
    end.

new_var_in_match(A) ->
    % second shouldn't match, var defined in pattern
    {Same, Same} = {1, A}.

map_key_match(M) ->
    case M of
        #{M := 1} ->
            ok;
        _ ->
            error
    end.

right_head_match(M, 1 = K) ->
    case M of
        #{K := 1} ->
            ok;
        _ ->
            error
    end.

match_on_right_side() ->
    K = 1,
    % match on right side doesn't add hat (1 = k)
    1 = K.

lc_pattern_vars_in_expr() ->
    % vars in pattern are in expr (body) scope, (^a = 1)
    [A = 1 || A <- [1]],
    % this var is new, vars in lc have their own scope (a = 1)
    A = 1,
    [{Mod, Id, State} || #handler{module = Mod, id = Id, state = State} <- []],
    A.

match_left_var_not_on_right_side() ->
    V =
        fun () ->
                % shouldn't match, right side before left
                V = 1,
                V
        end,
    V.

take(_Smaller, Larger) ->
    {Key, Value} = Larger,
    Key + Value.
