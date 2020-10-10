-module(ast).

%% An map transformer of Erlang abstract syntax based on erl_id_trans.erl

%% This module only traverses legal Erlang code. This is most noticeable
%% in guards where only a limited number of expressions are allowed.
%% N.B. if this module is to be used as a basis for transforms then
%% all the error cases must be handled otherwise this module just crashes!

-export([map/3, expr/3, reduce/4]).

-export([clause/3]).

map(Forms, St, MapFn) when is_list(Forms) ->
    forms(Forms, St, MapFn);

map(Form, St, MapFn) ->
    form(Form, St, MapFn).

leaf(Ast, St, Fn) ->
    case Fn(Ast, St) of
        auto ->
            {Ast, St};
        {auto, St1} ->
            {Ast, St1};
        {ok, Ast1, St1} ->
            {Ast1, St1}
    end.

node(Ast, St, Fn, AutoFn) ->
    case Fn(Ast, St) of
        auto ->
            AutoFn(Ast, St, Fn);
        {auto, St1} ->
            AutoFn(Ast, St1, Fn);
        {ok, Ast1, St1} ->
            {Ast1, St1}
    end.

reduce(Asts, St, Fn, RFn) ->
    reduce(Asts, St, Fn, RFn, []).

reduce([], St, _Fn, _RFn, Accum) ->
    {lists:reverse(Accum), St};
reduce([Ast | Asts], St, Fn, RFn, Accum) ->
    {NewAst, NewSt} = RFn(Ast, St, Fn),
    reduce(Asts, NewSt, Fn, RFn, [NewAst | Accum]).

forms(Forms, St, Fn) ->
    reduce(Forms, St, Fn, fun form/3).

%% -type form(Form) -> Form.
%%  Here we show every known form and valid internal structure. We do not
%%  that the ordering is correct!

%% First the various attributes.
form({attribute, Line, module, Mod}, St, Fn) ->
    leaf({attribute, Line, module, Mod}, St, Fn);
form({attribute, Line, file, {File, Line}},
     St,
     Fn) ->      %This is valid anywhere.
    leaf({attribute, Line, file, {File, Line}}, St, Fn);
form(Ast = {attribute, Line, export, Es0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Es1, St1} = farity_list(Es0, St, Fn),
                 {{attribute, Line, export, Es1}, St1}
         end);
form(Ast = {attribute, Line, import, {Mod, Is0}}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Is1, St1} = farity_list(Is0, St, Fn),
                 {{attribute, Line, import, {Mod, Is1}}, St1}
         end);
form(Ast = {attribute, Line, export_type, Es0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Es1, St1} = farity_list(Es0, St, Fn),
                 {{attribute, Line, export_type, Es1}, St1}
         end);
form(Ast = {attribute, Line, optional_callbacks, Es0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 try farity_list(Es0, St, Fn) of
                     {Es1, St1} ->
                         {{attribute, Line, optional_callbacks, Es1}, St1}
                 catch
                     _:_ ->
                         {{attribute, Line, optional_callbacks, Es0}, St}
                 end
         end);
form(Ast = {attribute, _Line, compile, _C}, St, Fn) ->
    leaf(Ast, St, Fn);
form(Ast = {attribute, Line, record, {Name, Defs0}}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Defs1, St1} = record_defs(Defs0, St, Fn),
                 {{attribute, Line, record, {Name, Defs1}}, St1}
         end);
form(Ast = {attribute, _Line, asm, {function, _N, _A, _Code}}, St, Fn) ->
    leaf(Ast, St, Fn);
form(Ast = {attribute, Line, type, {N, T, Vs}}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {T1, St1} = type(T, St, Fn),
                 {Vs1, St2} = variable_list(Vs, St1, Fn),
                 {{attribute, Line, type, {N, T1, Vs1}}, St2}
         end);
form(Ast = {attribute, Line, opaque, {N, T, Vs}}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {T1, St1} = type(T, St, Fn),
                 {Vs1, St2} = variable_list(Vs, St1, Fn),
                 {{attribute, Line, opaque, {N, T1, Vs1}}, St2}
         end);
form(Ast = {attribute, Line, spec, {{N, A}, FTs}}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {FTs1, St1} = function_type_list(FTs, St, Fn),
                 {{attribute, Line, spec, {{N, A}, FTs1}}, St1}
         end);
form(Ast = {attribute, Line, spec, {{M, N, A}, FTs}}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {FTs1, St1} = function_type_list(FTs, St, Fn),
                 {{attribute, Line, spec, {{M, N, A}, FTs1}}, St1}
         end);
form(Ast = {attribute, Line, callback, {{N, A}, FTs}}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {FTs1, St1} = function_type_list(FTs, St, Fn),
                 {{attribute, Line, callback, {{N, A}, FTs1}}, St1}
         end);
form(Ast = {attribute, _Line, _Attr, _Val},
     St,
     Fn) ->               %The general attribute.
    leaf(Ast, St, Fn);
form(Ast = {function, Line, Name0, Arity0, Clauses0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {{Name, Arity, Clauses}, St1} = function(Name0, Arity0, Clauses0, St, Fn),
                 {{function, Line, Name, Arity, Clauses}, St1}
         end);
%% Extra forms from the parser.
form(Ast = {error, _E}, St, Fn) ->
    leaf(Ast, St, Fn);
form(Ast = {warning, _W}, St, Fn) ->
    leaf(Ast, St, Fn);
form(Ast = {eof, _Line}, St, Fn) ->
    leaf(Ast, St, Fn).

%% -type farity_list(Ast=[Farity], St, Fn) -> [Farity] when Farity <= {atom(),integer()}.

farity_list(Fas, St, Fn) ->
    reduce(Fas, St, Fn, fun farity/3).

farity(Ast = {_Name, _Arity}, St, Fn) ->
    leaf(Ast, St, Fn).

%% -type variable_list(Ast=[Var], St, Fn) -> [Var]

variable_list(Vs, St, Fn) ->
    reduce(Vs, St, Fn, fun variable/3).

variable(Ast = {var, _Line, _Var}, St, Fn) ->
    leaf(Ast, St, Fn).

%% -type record_defs(Ast=[RecDef], St, Fn) -> [RecDef].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *parser*!

record_defs(Is, St, Fn) ->
    reduce(Is, St, Fn, fun record_def/3).

record_def(Ast = {record_field, Line, {atom, La, A}, Val0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Val1, St1} = expr(Val0, St, Fn),
                 {{record_field, Line, {atom, La, A}, Val1}, St1}
         end);
record_def(Ast = {record_field, _Line, {atom, _La, _A}}, St, Fn) ->
    leaf(Ast, St, Fn);
record_def(Ast = {typed_record_field, {record_field, Line, {atom, La, A}, Val0}, Type},
           St,
           Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Val1, St1} = expr(Val0, St, Fn),
                 {Type1, St2} = type(Type, St1, Fn),
                 {{typed_record_field, {record_field, Line, {atom, La, A}, Val1}, Type1}, St2}
         end);
record_def(Ast = {typed_record_field, {record_field, Line, {atom, La, A}}, Type},
           St,
           Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Type1, St1} = type(Type, St, Fn),
                 {{typed_record_field, {record_field, Line, {atom, La, A}}, Type1}, St1}
         end).

%% -type function(atom(), integer(), [Clause], St, Fn) -> {atom(),integer(Ast=),[Clause]}.

function(Name, Arity, Clauses0, St, Fn) ->
    Ast = {Name, Arity, Clauses0},
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Clauses1, St1} = clauses(Clauses0, St, Fn),
                 {{Name, Arity, Clauses1}, St1}
         end).

%% -type clauses(Ast=[Clause], St, Fn) -> [Clause].

clauses(Cs, St, Fn) ->
    reduce(Cs, St, Fn, fun clause/3).

%% -type clause(Ast=Clause, St, Fn) -> Clause.

clause(Ast = {clause, Line, H0, G0, B0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {H1, St1} = head(H0, St, Fn),
                 {G1, St2} = guard(G0, St1, Fn),
                 {B1, St3} = exprs(B0, St2, Fn),
                 {{clause, Line, H1, G1, B1}, St3}
         end).

%% -type head(Ast=[Pattern], St, Fn) -> [Pattern].

head(Ps, St, Fn) ->
    patterns(Ps, St, Fn).

%% -type patterns(Ast=[Pattern], St, Fn) -> [Pattern].
%%  These patterns are processed "sequentially" for purposes of variable
%%  definition etc.

patterns(Ps, St, Fn) ->
    reduce(Ps, St, Fn, fun pattern/3).

%% -type pattern(Pattern, St, Fn) -> Pattern.
%%  N.B. Only valid patterns are included here.

pattern(Ast = {var, _Line, _V}, St, Fn) ->
    leaf(Ast, St, Fn);
pattern(Ast = {match, Line, L0, R0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {L1, St1} = pattern(L0, St, Fn),
                 {R1, St2} = pattern(R0, St1, Fn),
                 {{match, Line, L1, R1}, St2}
         end);
pattern(Ast = {integer, _Line, _I}, St, Fn) ->
    leaf(Ast, St, Fn);
pattern(Ast = {char, _Line, _C}, St, Fn) ->
    leaf(Ast, St, Fn);
pattern(Ast = {float, _Line, _F}, St, Fn) ->
    leaf(Ast, St, Fn);
pattern(Ast = {atom, _Line, _A}, St, Fn) ->
    leaf(Ast, St, Fn);
pattern(Ast = {string, _Line, _S}, St, Fn) ->
    leaf(Ast, St, Fn);
pattern(Ast = {nil, _Line}, St, Fn) ->
    leaf(Ast, St, Fn);
pattern(Ast = {cons, Line, H0, T0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {H1, St1} = pattern(H0, St, Fn),
                 {T1, St2} = pattern(T0, St1, Fn),
                 {{cons, Line, H1, T1}, St2}
         end);
pattern(Ast = {tuple, Line, Ps0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Ps1, St1} = pattern_list(Ps0, St, Fn),
                 {{tuple, Line, Ps1}, St1}
         end);
pattern(Ast = {map, Line, Ps0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Ps1, St1} = pattern_list(Ps0, St, Fn),
                 {{map, Line, Ps1}, St1}
         end);
pattern(Ast = {map_field_exact, Line, K, V}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Ke, St1} = expr(K, St, Fn),
                 {Ve, St2} = pattern(V, St1, Fn),
                 {{map_field_exact, Line, Ke, Ve}, St2}
         end);
pattern(Ast = {record, Line, Name, Pfs0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Pfs1, St1} = pattern_fields(Pfs0, St, Fn),
                 {{record, Line, Name, Pfs1}, St1}
         end);
pattern(Ast = {record_index, Line, Name, Field0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Field1, St1} = pattern(Field0, St, Fn),
                 {{record_index, Line, Name, Field1}, St1}
         end);
pattern(Ast = {record_field, Line, Rec0, Name, Field0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Rec1, St1} = expr(Rec0, St, Fn),
                 {Field1, St2} = expr(Field0, St1, Fn),
                 {{record_field, Line, Rec1, Name, Field1}, St2}
         end);
pattern(Ast = {record_field, Line, Rec0, Field0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Rec1, St1} = expr(Rec0, St, Fn),
                 {Field1, St2} = expr(Field0, St1, Fn),
                 {{record_field, Line, Rec1, Field1}, St2}
         end);
pattern(Ast = {bin, Line, Fs}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Fs2, St1} = pattern_grp(Fs, St, Fn),
                 {{bin, Line, Fs2}, St1}
         end);
pattern(Ast = {op, _Line, _Op, _A}, St, Fn) ->
    leaf(Ast, St, Fn);
pattern(Ast = {op, _Line, _Op, _L, _R}, St, Fn) ->
    leaf(Ast, St, Fn).

pattern_grp(Fs, St, Fn) ->
    reduce(Fs, St, Fn, fun pattern_grp_item/3).

pattern_grp_item({bin_element, L1, E1, S1, T1}, St, Fn) ->
    {S2, St1} =
        case S1 of
            default ->
                default;
            _ ->
                expr(S1, St, Fn)
        end,
    {T2, St2} =
        case T1 of
            default ->
                default;
            _ ->
                bit_types(T1, St1, Fn)
        end,
    {E1_1, St3} = expr(E1, St2, Fn),
    {{bin_element, L1, E1_1, S2, T2}, St3}.

bit_types(BTs, St, Fn) ->
    reduce(BTs, St, Fn, fun bit_type/3).

bit_type(Atom, St, _Fn) when is_atom(Atom) ->
    {Atom, St};
bit_type({Atom, Integer}, St, _Fn) when is_atom(Atom), is_integer(Integer) ->
    {{Atom, Integer}, St}.

%% -type pattern_list([Pattern], St, Fn) -> [Pattern].
%%  These patterns are processed "in parallel" for purposes of variable
%%  definition etc.

pattern_list(Ps, St, Fn) ->
    reduce(Ps, St, Fn, fun pattern/3).

%% -type pattern_fields([Field], St, Fn) -> [Field].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

pattern_fields(Pfs, St, Fn) ->
    reduce(Pfs, St, Fn, fun pattern_field/3).

pattern_field(Ast = {record_field, Lf, {atom, La, F}, P0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {P1, St1} = pattern(P0, St, Fn),
                 {{record_field, Lf, {atom, La, F}, P1}, St1}
         end);
pattern_field(Ast = {record_field, Lf, {var, La, '_'}, P0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {P1, St1} = pattern(P0, St, Fn),
                 {{record_field, Lf, {var, La, '_'}, P1}, St1}
         end).

%% -type guard([GuardTest], St, Fn) -> [GuardTest].

guard(L = [G0 | _Gs], St, Fn) when is_list(G0) ->
    reduce(L, St, Fn, fun guard0/3);
guard(L, St, Fn) ->
    guard0(L, St, Fn).

guard0(Gs, St, Fn) ->
    reduce(Gs, St, Fn, fun guard_test/3).

guard_test(Ast = {call, Line, {atom, La, F}, As0}, St, Fn) ->
    case erl_internal:type_test(F, length(As0)) of
        true ->
            node(Ast,
                 St,
                 Fn,
                 fun(_Ast, St, Fn) ->
                         {As1, St1} = gexpr_list(As0, St, Fn),
                         {{call, Line, {atom, La, F}, As1}, St1}
                 end);
        _ ->
            gexpr(Ast, St, Fn)
    end;
guard_test(Any, St, Fn) ->
    gexpr(Any, St, Fn).

%% Before R9, there were special rules regarding the expressions on
%% top level in guards. Those limitations are now lifted - therefore
%% there is no need for a special clause for the toplevel expressions.
%% -type gexpr(GuardExpr, St, Fn) -> GuardExpr.

gexpr(Ast = {var, _Line, _V}, St, Fn) ->
    leaf(Ast, St, Fn);
gexpr(Ast = {integer, _Line, _I}, St, Fn) ->
    leaf(Ast, St, Fn);
gexpr(Ast = {char, _Line, _C}, St, Fn) ->
    leaf(Ast, St, Fn);
gexpr(Ast = {float, _Line, _F}, St, Fn) ->
    leaf(Ast, St, Fn);
gexpr(Ast = {atom, _Line, _A}, St, Fn) ->
    leaf(Ast, St, Fn);
gexpr(Ast = {string, _Line, _S}, St, Fn) ->
    leaf(Ast, St, Fn);
gexpr(Ast = {nil, _Line}, St, Fn) ->
    leaf(Ast, St, Fn);
gexpr(Ast = {map, Line, Map0, Es0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {[Map1 | Es1], St1} = gexpr_list([Map0 | Es0], St, Fn),
                 {{map, Line, Map1, Es1}, St1}
         end);
gexpr(Ast = {map, Line, Es0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Es1, St1} = gexpr_list(Es0, St, Fn),
                 {{map, Line, Es1}, St1}
         end);
gexpr(Ast = {map_field_assoc, Line, K, V}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Ke, St1} = gexpr(K, St, Fn),
                 {Ve, St2} = gexpr(V, St1, Fn),
                 {{map_field_assoc, Line, Ke, Ve}, St2}
         end);
gexpr(Ast = {map_field_exact, Line, K, V}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Ke, St1} = gexpr(K, St, Fn),
                 {Ve, St2} = gexpr(V, St1, Fn),
                 {{map_field_exact, Line, Ke, Ve}, St2}
         end);
gexpr(Ast = {cons, Line, H0, T0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {H1, St1} = gexpr(H0, St, Fn),
                 %They see the same variables
                 {T1, St2} = gexpr(T0, St1, Fn),
                 {{cons, Line, H1, T1}, St2}
         end);
gexpr(Ast = {tuple, Line, Es0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Es1, St1} = gexpr_list(Es0, St, Fn),
                 {{tuple, Line, Es1}, St1}
         end);
gexpr(Ast = {record_index, Line, Name, Field0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Field1, St1} = gexpr(Field0, St, Fn),
                 {{record_index, Line, Name, Field1}, St1}
         end);
gexpr(Ast = {record_field, Line, Rec0, Name, Field0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Rec1, St1} = gexpr(Rec0, St, Fn),
                 {Field1, St2} = gexpr(Field0, St1, Fn),
                 {{record_field, Line, Rec1, Name, Field1}, St2}
         end);
gexpr(Ast = {record, Line, Name, Inits0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Inits1, St1} = grecord_inits(Inits0, St, Fn),
                 {{record, Line, Name, Inits1}, St1}
         end);
gexpr(Ast = {call, Line, {atom, La, F}, As0}, St, Fn) ->
    case erl_internal:guard_bif(F, length(As0)) of
        true ->
            node(Ast,
                 St,
                 Fn,
                 fun(_Ast, St, Fn) ->
                         {As1, St1} = gexpr_list(As0, St, Fn),
                         {{call, Line, {atom, La, F}, As1}, St1}
                 end)
    end;
% Guard bif's can be remote, but only in the module erlang...
gexpr(Ast = {call, Line, {remote, La, {atom, Lb, erlang}, {atom, Lc, F}}, As0}, St, Fn) ->
    case erl_internal:guard_bif(F, length(As0)) or erl_internal:arith_op(F, length(As0)) or
             erl_internal:comp_op(F, length(As0))
             or erl_internal:bool_op(F, length(As0))
        of
        true ->
            node(Ast,
                 St,
                 Fn,
                 fun(_Ast, St, Fn) ->
                         {As1, St1} = gexpr_list(As0, St, Fn),
                         {{call, Line, {remote, La, {atom, Lb, erlang}, {atom, Lc, F}}, As1}, St1}
                 end)
    end;
gexpr(Ast = {bin, Line, Fs}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Fs2, St1} = pattern_grp(Fs, St, Fn),
                 {{bin, Line, Fs2}, St1}
         end);
gexpr(Ast = {op, Line, Op, A0}, St, Fn) ->
    case erl_internal:arith_op(Op, 1) or erl_internal:bool_op(Op, 1) of
        true ->
            node(Ast,
                 St,
                 Fn,
                 fun(_Ast, St, Fn) ->
                         {A1, St1} = gexpr(A0, St, Fn),
                         {{op, Line, Op, A1}, St1}
                 end)
    end;
gexpr(Ast = {op, Line, Op, L0, R0}, St, Fn) when Op =:= 'andalso'; Op =:= 'orelse' ->
    %% R11B: andalso/orelse are now allowed in guards.
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {L1, St1} = gexpr(L0, St, Fn),
                 %They see the same variables
                 {R1, St2} = gexpr(R0, St1, Fn),
                 {{op, Line, Op, L1, R1}, St2}
         end);
gexpr(Ast = {op, Line, Op, L0, R0}, St, Fn) ->
    case erl_internal:arith_op(Op, 2) or erl_internal:bool_op(Op, 2) or
             erl_internal:comp_op(Op, 2)
        of
        true ->
            node(Ast,
                 St,
                 Fn,
                 fun(_Ast, St, Fn) ->
                         {L1, St1} = gexpr(L0, St, Fn),
                         {R1, St2} = gexpr(R0, St1, Fn),
                         {{op, Line, Op, L1, R1}, St2}
                 end)
    end.

%% -type gexpr_list([GuardExpr], St, Fn) -> [GuardExpr].
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

gexpr_list(Es, St, Fn) ->
    reduce(Es, St, Fn, fun gexpr/3).

grecord_inits(Is, St, Fn) ->
    reduce(Is, St, Fn, fun grecord_init/3).

grecord_init(Ast = {record_field, Lf, {atom, La, F}, Val0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Val1, St1} = gexpr(Val0, St, Fn),
                 {{record_field, Lf, {atom, La, F}, Val1}, St1}
         end);
grecord_init(Ast = {record_field, Lf, {var, La, '_'}, Val0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Val1, St1} = gexpr(Val0, St, Fn),
                 {{record_field, Lf, {var, La, '_'}, Val1}, St1}
         end).

%% -type exprs([Expression], St, Fn) -> [Expression].
%%  These expressions are processed "sequentially" for purposes of variable
%%  definition etc.

exprs(Es, St, Fn) ->
    reduce(Es, St, Fn, fun expr/3).

%% -type expr(Expression, St, Fn) -> Expression.

expr(Ast = {var, _Line, _V}, St, Fn) ->
    leaf(Ast, St, Fn);
expr(Ast = {integer, _Line, _I}, St, Fn) ->
    leaf(Ast, St, Fn);
expr(Ast = {float, _Line, _F}, St, Fn) ->
    leaf(Ast, St, Fn);
expr(Ast = {atom, _Line, _A}, St, Fn) ->
    leaf(Ast, St, Fn);
expr(Ast = {string, _Line, _S}, St, Fn) ->
    leaf(Ast, St, Fn);
expr(Ast = {char, _Line, _C}, St, Fn) ->
    leaf(Ast, St, Fn);
expr(Ast = {nil, _Line}, St, Fn) ->
    leaf(Ast, St, Fn);
expr(Ast = {cons, Line, H0, T0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {H1, St1} = expr(H0, St, Fn),
                 %They see the same variables
                 {T1, St2} = expr(T0, St1, Fn),
                 {{cons, Line, H1, T1}, St2}
         end);
expr(Ast = {lc, Line, E0, Qs0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Qs1, St1} = lc_bc_quals(Qs0, St, Fn),
                 {E1, St2} = expr(E0, St1, Fn),
                 {{lc, Line, E1, Qs1}, St2}
         end);
expr(Ast = {bc, Line, E0, Qs0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Qs1, St1} = lc_bc_quals(Qs0, St, Fn),
                 {E1, St2} = expr(E0, St1, Fn),
                 {{bc, Line, E1, Qs1}, St2}
         end);
expr(Ast = {tuple, Line, Es0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Es1, St1} = expr_list(Es0, St, Fn),
                 {{tuple, Line, Es1}, St1}
         end);
expr(Ast = {map, Line, Map0, Es0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {[Map1 | Es1], St1} = exprs([Map0 | Es0], St, Fn),
                 {{map, Line, Map1, Es1}, St1}
         end);
expr(Ast = {map, Line, Es0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Es1, St1} = exprs(Es0, St, Fn),
                 {{map, Line, Es1}, St1}
         end);
expr(Ast = {map_field_assoc, Line, K, V}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Ke, St1} = expr(K, St, Fn),
                 {Ve, St2} = expr(V, St1, Fn),
                 {{map_field_assoc, Line, Ke, Ve}, St2}
         end);
expr(Ast = {map_field_exact, Line, K, V}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Ke, St1} = expr(K, St, Fn),
                 {Ve, St2} = expr(V, St1, Fn),
                 {{map_field_exact, Line, Ke, Ve}, St2}
         end);
expr(Ast = {record_index, Line, Name, Field0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Field1, St1} = expr(Field0, St, Fn),
                 {{record_index, Line, Name, Field1}, St1}
         end);
expr(Ast = {record, Line, Name, Inits0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Inits1, St1} = record_inits(Inits0, St, Fn),
                 {{record, Line, Name, Inits1}, St1}
         end);
expr(Ast = {record_field, Line, Rec0, Name, Field0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Rec1, St1} = expr(Rec0, St, Fn),
                 {Field1, St2} = expr(Field0, St1, Fn),
                 {{record_field, Line, Rec1, Name, Field1}, St2}
         end);
expr(Ast = {record, Line, Rec0, Name, Upds0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Rec1, St1} = expr(Rec0, St, Fn),
                 {Upds1, St2} = record_updates(Upds0, St1, Fn),
                 {{record, Line, Rec1, Name, Upds1}, St2}
         end);
expr(Ast = {record_field, Line, Rec0, Field0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Rec1, St1} = expr(Rec0, St, Fn),
                 {Field1, St2} = expr(Field0, St1, Fn),
                 {{record_field, Line, Rec1, Field1}, St2}
         end);
expr(Ast = {block, Line, Es0}, St, Fn) ->
    %% Unfold block into a sequence.
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Es1, St1} = exprs(Es0, St, Fn),
                 {{block, Line, Es1}, St1}
         end);
expr(Ast = {'if', Line, Cs0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Cs1, St1} = icr_clauses(Cs0, St, Fn),
                 {{'if', Line, Cs1}, St1}
         end);
expr(Ast = {'case', Line, E0, Cs0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {E1, St1} = expr(E0, St, Fn),
                 {Cs1, St2} = icr_clauses(Cs0, St1, Fn),
                 {{'case', Line, E1, Cs1}, St2}
         end);
expr(Ast = {'receive', Line, Cs0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Cs1, St1} = icr_clauses(Cs0, St, Fn),
                 {{'receive', Line, Cs1}, St1}
         end);
expr(Ast = {'receive', Line, Cs0, To0, ToEs0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {To1, St1} = expr(To0, St, Fn),
                 {ToEs1, St2} = exprs(ToEs0, St1, Fn),
                 {Cs1, St3} = icr_clauses(Cs0, St2, Fn),
                 {{'receive', Line, Cs1, To1, ToEs1}, St3}
         end);
expr(Ast = {'try', Line, Es0, Scs0, Ccs0, As0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Es1, St1} = exprs(Es0, St, Fn),
                 {Scs1, St2} = icr_clauses(Scs0, St1, Fn),
                 {Ccs1, St3} = icr_clauses(Ccs0, St2, Fn),
                 {As1, St4} = exprs(As0, St3, Fn),
                 {{'try', Line, Es1, Scs1, Ccs1, As1}, St4}
         end);
expr(Ast = {'fun', Line, Body}, St, Fn) ->
    case Body of
        {clauses, Cs0} ->
            node(Ast,
                 St,
                 Fn,
                 fun(_Ast, St, Fn) ->
                         {Cs1, St1} = fun_clauses(Cs0, St, Fn),
                         {{'fun', Line, {clauses, Cs1}}, St1}
                 end);
        {function, F, A} ->
            leaf({'fun', Line, {function, F, A}}, St, Fn);
        {function, M, F, A} when is_atom(M), is_atom(F), is_integer(A) ->
            %% R10B-6: fun M:F/A. (Backward compatibility)
            leaf({'fun', Line, {function, M, F, A}}, St, Fn);
        {function, M0, F0, A0} ->
            %% R15: fun M:F/A with variables.
            node(Ast,
                 St,
                 Fn,
                 fun(_Ast, St, Fn) ->
                         {M, St1} = expr(M0, St, Fn),
                         {F, St2} = expr(F0, St1, Fn),
                         {A, St3} = expr(A0, St2, Fn),
                         {{'fun', Line, {function, M, F, A}}, St3}
                 end)
    end;
expr(Ast = {named_fun, Loc, Name, Cs}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Cs1, St1} = fun_clauses(Cs, St, Fn),
                 {{named_fun, Loc, Name, Cs1}, St1}
         end);
expr(Ast = {call, Line, F0, As0}, St, Fn) ->
    %% N.B. If F an atom then call to local function or BIF, if F a
    %% remote structure (see below) then call to other module,
    %% otherwise apply to "function".
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {F1, St1} = expr(F0, St, Fn),
                 {As1, St2} = expr_list(As0, St1, Fn),
                 {{call, Line, F1, As1}, St2}
         end);
expr(Ast = {'catch', Line, E0}, St, Fn) ->
    %% No new variables added.
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {E1, St1} = expr(E0, St, Fn),
                 {{'catch', Line, E1}, St1}
         end);
expr(Ast = {match, Line, P0, E0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {E1, St1} = expr(E0, St, Fn),
                 {P1, St2} = pattern(P0, St1, Fn),
                 {{match, Line, P1, E1}, St2}
         end);
expr(Ast = {bin, Line, Fs}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Fs2, St1} = pattern_grp(Fs, St, Fn),
                 {{bin, Line, Fs2}, St1}
         end);
expr(Ast = {op, Line, Op, A0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {A1, St1} = expr(A0, St, Fn),
                 {{op, Line, Op, A1}, St1}
         end);
expr(Ast = {op, Line, Op, L0, R0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {L1, St1} = expr(L0, St, Fn),
                 %They see the same variables
                 {R1, St2} = expr(R0, St1, Fn),
                 {{op, Line, Op, L1, R1}, St2}
         end);
%% The following are not allowed to occur anywhere!
expr(Ast = {remote, Line, M0, F0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {M1, St1} = expr(M0, St, Fn),
                 {F1, St2} = expr(F0, St1, Fn),
                 {{remote, Line, M1, F1}, St2}
         end).

%% -type expr_list([Expression], St, Fn) -> [Expression].
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

expr_list(Es, St, Fn) ->
    reduce(Es, St, Fn, fun expr/3).

%% -type record_inits([RecordInit], St, Fn) -> [RecordInit].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_inits(Is, St, Fn) ->
    reduce(Is, St, Fn, fun record_init/3).

record_init(Ast = {record_field, Lf, {atom, La, F}, Val0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Val1, St1} = expr(Val0, St, Fn),
                 {{record_field, Lf, {atom, La, F}, Val1}, St1}
         end);
record_init(Ast = {record_field, Lf, {var, La, '_'}, Val0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Val1, St1} = expr(Val0, St, Fn),
                 {{record_field, Lf, {var, La, '_'}, Val1}, St1}
         end).

%% -type record_updates([RecordUpd], St, Fn) -> [RecordUpd].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_updates(Us, St, Fn) ->
    reduce(Us, St, Fn, fun record_update/3).

record_update(Ast = {record_field, Lf, {atom, La, F}, Val0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Val1, St1} = expr(Val0, St, Fn),
                 {{record_field, Lf, {atom, La, F}, Val1}, St1}
         end).

%% -type icr_clauses([Clause], St, Fn) -> [Clause].

icr_clauses(Cs, St, Fn) ->
    reduce(Cs, St, Fn, fun icr_clause/3).

icr_clause(Ast, St, Fn) ->
    node(Ast, St, Fn, fun clause/3).

%% -type lc_bc_quals([Qualifier], St, Fn) -> [Qualifier].
%%  Allow filters to be both guard tests and general expressions.

lc_bc_quals(Qs, St, Fn) ->
    reduce(Qs, St, Fn, fun lc_bc_qual/3).

lc_bc_qual(Ast = {generate, Line, P0, E0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {E1, St1} = expr(E0, St, Fn),
                 {P1, St2} = pattern(P0, St1, Fn),
                 {{generate, Line, P1, E1}, St2}
         end);
lc_bc_qual(Ast = {b_generate, Line, P0, E0}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {E1, St1} = expr(E0, St, Fn),
                 {P1, St2} = pattern(P0, St1, Fn),
                 {{b_generate, Line, P1, E1}, St2}
         end);
lc_bc_qual(Ast, St, Fn) ->
    node(Ast, St, Fn, fun expr/3).

%% -type fun_clauses([Clause], St, Fn) -> [Clause].

fun_clauses(Cs, St, Fn) ->
    reduce(Cs, St, Fn, fun fun_clause/3).

fun_clause(Ast, St, Fn) ->
    node(Ast, St, Fn, fun clause/3).

function_type_list(Fts, St, Fn) ->
    reduce(Fts, St, Fn, fun function_type_list_item/3).

function_type_list_item(Ast = {type, Line, bounded_fun, [Ft, Fc]}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Ft1, St1} = function_type(Ft, St, Fn),
                 {Fc1, St2} = function_constraint(Fc, St1, Fn),
                 {{type, Line, bounded_fun, [Ft1, Fc1]}, St2}
         end);
function_type_list_item(Ft, St, Fn) ->
    function_type(Ft, St, Fn).

function_type(Ast = {type, Line, 'fun', [{type, Lt, product, As}, B]}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {As1, St1} = type_list(As, St, Fn),
                 {B1, St2} = type(B, St1, Fn),
                 {{type, Line, 'fun', [{type, Lt, product, As1}, B1]}, St2}
         end).

function_constraint(Cs, St, Fn) ->
    reduce(Cs, St, Fn, fun function_constraint_item/3).

function_constraint_item(Ast, St, Fn) ->
    node(Ast, St, Fn, fun constraint/3).

constraint(Ast = {type, Line, constraint, [{atom, L, A}, [V, T]]}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {V1, St1} = type(V, St, Fn),
                 {T1, St2} = type(T, St1, Fn),
                 {{type, Line, constraint, [{atom, L, A}, [V1, T1]]}, St2}
         end).

type(Ast = {ann_type, Line, [{var, Lv, V}, T]}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {T1, St1} = type(T, St, Fn),
                 {{ann_type, Line, [{var, Lv, V}, T1]}, St1}
         end);
type(Ast = {atom, _Line, _A}, St, Fn) ->
    leaf(Ast, St, Fn);
type(Ast = {integer, _Line, _I}, St, Fn) ->
    leaf(Ast, St, Fn);
type(Ast = {op, Line, Op, T}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {T1, St1} = type(T, St, Fn),
                 {{op, Line, Op, T1}, St1}
         end);
type(Ast = {op, Line, Op, L, R}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {L1, St1} = type(L, St, Fn),
                 {R1, St2} = type(R, St1, Fn),
                 {{op, Line, Op, L1, R1}, St2}
         end);
type(Ast = {type, Line, binary, [M, N]}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {M1, St1} = type(M, St, Fn),
                 {N1, St2} = type(N, St1, Fn),
                 {{type, Line, binary, [M1, N1]}, St2}
         end);
type(Ast = {type, _Line, 'fun', []}, St, Fn) ->
    leaf(Ast, St, Fn);
type(Ast = {type, Line, 'fun', [{type, Lt, any}, B]}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {B1, St1} = type(B, St, Fn),
                 {{type, Line, 'fun', [{type, Lt, any}, B1]}, St1}
         end);
type(Ast = {type, Line, range, [L, H]}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {L1, St1} = type(L, St, Fn),
                 {H1, St2} = type(H, St1, Fn),
                 {{type, Line, range, [L1, H1]}, St2}
         end);
type(Ast = {type, _Line, map, any}, St, Fn) ->
    leaf(Ast, St, Fn);
type(Ast = {type, Line, map, Ps}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Ps1, St1} = map_pair_types(Ps, St, Fn),
                 {{type, Line, map, Ps1}, St1}
         end);
type(Ast = {type, Line, record, [{atom, La, N} | Fs]}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Fs1, St1} = field_types(Fs, St, Fn),
                 {{type, Line, record, [{atom, La, N} | Fs1]}, St1}
         end);
type(Ast = {remote_type, Line, [{atom, Lm, M}, {atom, Ln, N}, As]}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {As1, St1} = type_list(As, St, Fn),
                 {{remote_type, Line, [{atom, Lm, M}, {atom, Ln, N}, As1]}, St1}
         end);
type(Ast = {type, _Line, tuple, any}, St, Fn) ->
    leaf(Ast, St, Fn);
type(Ast = {type, Line, tuple, Ts}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Ts1, St1} = type_list(Ts, St, Fn),
                 {{type, Line, tuple, Ts1}, St1}
         end);
type(Ast = {type, Line, union, Ts}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {Ts1, St1} = type_list(Ts, St, Fn),
                 {{type, Line, union, Ts1}, St1}
         end);
type(Ast = {var, _Line, _V}, St, Fn) ->
    leaf(Ast, St, Fn);
type(Ast = {user_type, Line, N, As}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {As1, St1} = type_list(As, St, Fn),
                 {{user_type, Line, N, As1}, St1}
         end);
type(Ast = {type, Line, N, As}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {As1, St1} = type_list(As, St, Fn),
                 {{type, Line, N, As1}, St1}
         end).

map_pair_types(Ps, St, Fn) ->
    reduce(Ps, St, Fn, fun map_pair_type/3).

map_pair_type(Ast = {type, Line, map_field_assoc, [K, V]}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {K1, St1} = type(K, St, Fn),
                 {V1, St2} = type(V, St1, Fn),
                 {{type, Line, map_field_assoc, [K1, V1]}, St2}
         end);
map_pair_type(Ast = {type, Line, map_field_exact, [K, V]}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {K1, St1} = type(K, St, Fn),
                 {V1, St2} = type(V, St1, Fn),
                 {{type, Line, map_field_exact, [K1, V1]}, St2}
         end).

field_types(Fs, St, Fn) ->
    reduce(Fs, St, Fn, fun field_type/3).

field_type(Ast = {type, Line, field_type, [{atom, La, A}, T]}, St, Fn) ->
    node(Ast,
         St,
         Fn,
         fun(_Ast, St, Fn) ->
                 {T1, St1} = type(T, St, Fn),
                 {{type, Line, field_type, [{atom, La, A}, T1]}, St1}
         end).

type_list(Ts, St, Fn) ->
    reduce(Ts, St, Fn, fun type_list_item/3).

type_list_item(Ast, St, Fn) ->
    node(Ast, St, Fn, fun type/3).
