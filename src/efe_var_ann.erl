-module(efe_var_ann).

-export([do/1]).
-export([map/2]).

new_scope() ->
    new_scope(#{}).

new_scope(Vars) ->
    #{vars => Vars}.

new_state() ->
    #{scope => new_scope(),
      scopes => [],
      matching => false,
      never_match => false}.

set_matching(S = #{never_match := true}) ->
    S;
set_matching(S) ->
    set_matching(S, true).

set_matching(S, Matching) ->
    S#{matching := Matching}.

clear_matching(S) ->
    S#{matching := false}.

set_never_match(S) ->
    S#{never_match := true}.

clear_never_match(S) ->
    S#{never_match := false}.

% http://icai.ektf.hu/pdf/ICAI2007-vol2-pp137-145.pdf
enter_scope(S = #{scope := Scope = #{vars := Vars}, scopes := Scopes}) ->
    S#{scope := new_scope(Vars), scopes := [Scope | Scopes]}.

add_var_if_not_there(S, _Line, '_') ->
    S;
add_var_if_not_there(S = #{scope := Scope = #{vars := Vars}}, Line, Name) ->
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
    {Left1, St1} = expr(Left, set_matching(St)),
    {Right1, _} = expr(Right, St),
    R = {match, Line, Left1, Right1},
    {ok, R, clear_matching(St1)};
% if doesn't match on head?
map({'if', Line, Cs0}, St) ->
    {Cs1, St1} = icr_clauses(Cs0, St),
    {ok, {'if', Line, Cs1}, St1};
map({'case', Line, E0, Cs0}, St) ->
    {E1, St1} = expr(E0, St),
    {Cs1, St2} = icr_clauses(Cs0, St1),
    {ok, {'case', Line, E1, Cs1}, St2};
map({'receive', Line, Cs0}, St) ->
    {Cs1, St1} = icr_clauses(Cs0, St),
    {ok, {'receive', Line, Cs1}, St1};
map({'receive', Line, Cs0, To0, ToEs0}, St) ->
    {To1, _St1} = expr(To0, St),
    {ToEs1, _St2} = exprs(ToEs0, St),
    {Cs1, _St3} = icr_clauses(Cs0, St),
    {ok, {'receive', Line, Cs1, To1, ToEs1}, St};
map({'try', Line, Es0, Scs0, Ccs0, As0}, St) ->
    {Es1, St1} = exprs(Es0, St),
    {Scs1, St2} = icr_clauses(Scs0, St1),
    {Ccs1, St3} = icr_clauses(Ccs0, St2),
    {As1, St4} = exprs(As0, St3),
    {ok, {'try', Line, Es1, Scs1, Ccs1, As1}, St4};
% new scopes
map({attribute, Line, spec, {{N, A}, FTs}}, St) ->
    % ignore vars introduced in specs
    Fn = fun map/2,
    {FTs1, _St1} = ast:function_type_list(FTs, St, Fn),
    {ok, {attribute, Line, spec, {{N, A}, FTs1}}, St};
map({attribute, Line, spec, {{M, N, A}, FTs}}, St) ->
    % ignore vars introduced in specs
    Fn = fun map/2,
    {FTs1, _St1} = ast:function_type_list(FTs, St, Fn),
    {ok, {attribute, Line, spec, {{M, N, A}, FTs1}}, St};
map({attribute, Line, type, {N, T, Vs}}, St) ->
    % ignore vars introduced in types
    Fn = fun map/2,
    {T1, St1} = ast:type(T, St, Fn),
    {Vs1, _St2} = ast:variable_list(Vs, St1, Fn),
    {ok, {attribute, Line, type, {N, T1, Vs1}}, St};
map({'fun', Line, {clauses, Cs0}}, St) ->
    {Cs1, St1} = fun_clauses(Cs0, St),
    {ok, {'fun', Line, {clauses, Cs1}}, St1};
map({named_fun, Loc, Name, Cs}, St) ->
    {Cs1, St1} = fun_clauses(Cs, St),
    {ok, {named_fun, Loc, Name, Cs1}, St1};
map({lc, Line, E0, Qs0}, St) ->
    {Qs1, St1} = lc_bc_quals(Qs0, St),
    {E1, _St2} = expr(E0, St1),
    % discard all information inside a list comprehension
    {ok, {lc, Line, E1, Qs1}, St};
map({bc, Line, E0, Qs0}, St) ->
    {Qs1, St1} = lc_bc_quals(Qs0, St),
    {E1, _St2} = expr(E0, St1),
    % discard all information inside a binary comprehension
    {ok, {bc, Line, E1, Qs1}, St};
map({var, Line, Name}, St = #{matching := Matching}) ->
    R =
        case get_var(St, Name) of
            nil ->
                {var,
                 Line,
                 Name,
                 #{new => true, defined => Line, matching => Matching}};
            #{line := DLine} ->
                {var,
                 Line,
                 Name,
                 #{new => false, defined => DLine, matching => Matching}}
        end,

    St1 = add_var_if_not_there(St, Line, Name),
    {ok, R, St1};
map({function, Line, Name0, Arity0, Clauses0}, St) ->
    {{Name, Arity, Clauses}, _St1} = function(Name0, Arity0, Clauses0, St),
    R = {function, Line, Name, Arity, Clauses},
    {ok, R, St};
% binary types aren't in match position
map({bin, Line, Fs}, St) ->
    {Fs2, St1} = pattern_grp(Fs, St),
    {ok, {bin, Line, Fs2}, St1};
map(_Ast, _St) ->
    auto.

function(Name, Arity, Clauses0, St) ->
    {Clauses1, _St1} = ast:reduce(Clauses0, St, fun map/2, fun fun_clause/3),
    {{Name, Arity, Clauses1}, St}.

expr(A, S) ->
    ast:expr(A, S, fun map/2).

exprs(A, S) ->
    ast:exprs(A, S, fun map/2).

icr_clauses(Cs, St) ->
    ast:reduce(Cs, St, fun map/2, fun match_clause/3).

fun_clauses(Cs, St) ->
    {Cs1, _St1} = ast:reduce(Cs, St, fun map/2, fun fun_clause/3),
    % discard all information inside an inline fun
    {Cs1, St}.

fun_clause({clause, Line, H0, G0, B0}, St, Fn) ->
    % don't set_matching in head since we don't need it in function head
    {H1, St1} = ast:head(H0, set_never_match(St), Fn),
    {G1, St2} = ast:guard(G0, clear_never_match(St1), Fn),
    {B1, _St3} = ast:exprs(B0, St2, Fn),
    {{clause, Line, H1, G1, B1}, St}.

match_clause({clause, Line, H0, G0, B0}, St, Fn) ->
    {H1, St1} = ast:head(H0, set_matching(St), Fn),
    {G1, St2} = ast:guard(G0, clear_matching(St1), Fn),
    {B1, _St3} = ast:exprs(B0, St2, Fn),
    {{clause, Line, H1, G1, B1}, St}.

lc_bc_quals(Qs, St) ->
    ast:reduce(Qs, St, fun map/2, fun lc_bc_qual/3).

lc_bc_qual({generate, Line, P0, E0}, St, Fn) ->
    {E1, St1} = ast:expr(E0, enter_scope(St), Fn),
    {P1, St2} = ast:pattern(P0, St1, Fn),
    {{generate, Line, P1, E1}, St2};
lc_bc_qual({b_generate, Line, P0, E0}, St, Fn) ->
    {E1, St1} = ast:expr(E0, St, Fn),
    {P1, St2} = ast:pattern(P0, St1, Fn),
    {{b_generate, Line, P1, E1}, St2};
lc_bc_qual(Ast, St, Fn) ->
    ast:expr(Ast, St, Fn).

pattern_grp(Fs, St) ->
    ast:reduce(Fs, St, fun map/2, fun pattern_grp_item/3).

% bin_element, Line, E1: Pattern, S1: Size, T1: Type Specifier List
pattern_grp_item({bin_element, L1, E1, S1, T1}, St, Fn) ->
    #{matching := WasMatching} = St,
    {S2, St1_0} =
        case S1 of
            default ->
                {default, St};
            _ ->
                % disable matching for
                ast:expr(S1, clear_matching(St), Fn)
        end,

    St1 = set_matching(St1_0, WasMatching),
    {T2, St2} =
        case T1 of
            default ->
                {default, St1};
            _ ->
                ast:bit_types(T1, St1, Fn)
        end,
    {E1_1, St3} = ast:expr(E1, St2, Fn),
    {{bin_element, L1, E1_1, S2, T2}, St3}.
