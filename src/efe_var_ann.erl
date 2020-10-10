-module(efe_var_ann).
-export([do/1]).
-export([map/2]).

-define(Enter(A, S), ast:expr(A, S, fun map/2)).

new_scope() -> #{vars => #{}}.

new_state() ->
    #{scope => new_scope(), scopes => [], matching => false}.

set_matching(S) ->
    S#{matching := true}.

clear_matching(S) ->
    S#{matching := false}.

enter_scope(S=#{scope := Scope, scopes := Scopes}) ->
    S#{scope := new_scope(), scopes := [Scope | Scopes]}.

add_var_if_not_there(S=#{scope := Scope=#{vars := Vars}}, Line, Name) ->
    case maps:get(Name, Vars, nil) of
        nil ->
            NewVars = Vars#{Name => #{line => Line}},
            S#{scope => Scope#{vars := NewVars}};
        _ ->
            S
    end.

get_var(#{scope := #{vars := Vars}}, Name) ->
    maps:get(Name, Vars, nil).


do(Ast) ->
    ast:map(Ast, new_state(), fun map/2).

map({match, Line, Left, Right}, St) ->
    {Left1, St1} = ?Enter(Left, set_matching(St)),
    {Right1, _} = ?Enter(Right, St),
    R = {match, Line, Left1, Right1},
    {ok, R, clear_matching(St1)};
map({var, Line, Name}, St=#{matching := Matching}) ->
    R = case get_var(St, Name) of
            nil -> {var, Line, Name, #{new => true, defined => Line, matching => Matching}};
            #{line := DLine} -> {var, Line, Name, #{new => false, defined => DLine, matching => Matching}}
        end,

    St1 = add_var_if_not_there(St, Line, Name),
    {ok, R, St1};

map({function, Line, Name0, Arity0, Clauses0}, St) ->
    {{Name, Arity, Clauses}, _St1} = function(Name0, Arity0, Clauses0, St),
    R = {function, Line, Name, Arity, Clauses},
    {ok, R, St};

map(_Ast, _St) ->
    auto.

function(Name, Arity, Clauses0, St) ->
    {Clauses1, _St1} = ast:reduce(Clauses0, St, fun map/2, fun clause/3),
    {{Name, Arity, Clauses1}, St}.

clause(Cs, St, Fn) ->
    ast:clause(Cs, enter_scope(St), Fn).
