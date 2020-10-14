%% Copyright 2019 Mariano Guerra
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(efe_pp).

-export([format/1, layout/1]).

-import(prettypr,
        [sep/1,
         beside/2,
         empty/0,
         text/1,
         floating/1,
         nest/2,
         par/2,
         above/2,
         follow/3]).
-import(erl_parse, [inop_prec/1, preop_prec/1]).

% used on tests
-export([pp_guards/2, default_ctx/0]).

-define(PADDING, 2).
-define(PAPER, 80). % 80
-define(RIBBON, 56). % 56
-define(NOUSER, undefined).
-define(NOHOOK, none).

-record(ctxt,
        {prec = 0 :: integer(),
         sub_indent = 2 :: non_neg_integer(),
         exports_all = false,
         exports = #{},
         records = #{},
         record_imported = false,
         stacktrace_varname = nil,
         break_indent = 4 :: non_neg_integer(),
         paper = ?PAPER :: integer(),
         ribbon = ?RIBBON :: integer()}).

layout(V) ->
    layout(V, default_ctx()).

layout(V, Ctx) when is_list(V) ->
    pp_mod(V, Ctx);
layout(V, Ctx) ->
    pp(V, Ctx).

format(V) ->
    prettypr:format(layout(V), ?PAPER, ?RIBBON).

default_ctx() ->
    #ctxt{}.

set_prec(Ctxt, Prec) ->
    Ctxt#ctxt{prec = Prec}.    % used internally

reset_prec(Ctxt) ->
    set_prec(Ctxt, 0).    % used internally

% don't set it if not explicitly set
set_stacktrace_var(Ctx, '_') ->
    Ctx;
set_stacktrace_var(Ctx, STraceVarName) ->
    Ctx#ctxt{stacktrace_varname = STraceVarName}.

pp_mod([], _Ctx) ->
    empty();
pp_mod([{attribute, _, module, ModName} | Nodes], Ctx) ->
    above(text("defmodule :m_" ++ a2l(ModName) ++ " do"),
          above(nestc(Ctx, pp_mod(Nodes, Ctx)), text("end")));
pp_mod([Node = {attribute, _, record, {RecName, Fields}} | Nodes], Ctx) ->
    Ctx1 = add_record_declaration(RecName, Fields, Ctx),
    {Ctx2, Cont} =
        case Ctx1#ctxt.record_imported of
            true ->
                {Ctx1, empty()};
            false ->
                {Ctx1#ctxt{record_imported = true}, text("require Record")}
        end,
    abovel([Cont, pp(Node, Ctx2), pp_mod(Nodes, Ctx2)]);
pp_mod([Node | Nodes], Ctx) ->
    Ctx1 = maybe_update_ctx(Node, Ctx),
    above(pp(Node, Ctx1), pp_mod(Nodes, Ctx1)).

maybe_update_ctx({attribute, _, compile, export_all}, Ctx) ->
    Ctx1 = Ctx#ctxt{exports_all = true},
    Ctx1;
maybe_update_ctx({attribute, _, export, Exports},
                 Ctx = #ctxt{exports = CurExports}) ->
    NewExports =
        maps:merge(CurExports, maps:from_list([{K, true} || K <- Exports])),
    Ctx1 = Ctx#ctxt{exports = NewExports},
    Ctx1;
maybe_update_ctx(_Node, Ctx) ->
    Ctx.

pp({error, _}, _Ctx) ->
    empty();
% TODO: handle specs
pp({attribute, _, spec, _}, _Ctx) ->
    empty();
% TODO: handle type
pp({attribute, _, type, _}, _Ctx) ->
    empty();
% TODO: handle opaque
pp({attribute, _, opaque, _}, _Ctx) ->
    empty();
pp({attribute, _, record, {RecName, []}}, _Ctx) ->
    besidel([text("Record.defrecord(:"),
             p_rec_name(RecName),
             text(", "),
             quote_atom(RecName),
             text(")")]);
pp({attribute, _, record, {RecName, Fields}}, Ctx) ->
    besidel([text("Record.defrecord(:"),
             p_rec_name(RecName),
             text(", "),
             quote_atom(RecName),
             text(", "),
             join(Fields, Ctx, fun pp_record_field_decl/2, comma_f()),
             text(")")]);
% TODO: handle dialyzer
pp({attribute, _, dialyzer, _}, _Ctx) ->
    empty();
% TODO: handle callback
pp({attribute, _, callback, _}, _Ctx) ->
    empty();
% TODO:
pp({attribute, _, removed, _}, _Ctx) ->
    empty();
% TODO: handle optional_callbacks
pp({attribute, _, optional_callbacks, _}, _Ctx) ->
    empty();
pp({attribute, _, file, _}, _Ctx) ->
    empty();
pp({attribute, _, Attr = behaviour, V}, _Ctx) ->
    gen_attr(Attr, V);
pp({attribute, _, behavior, V}, _Ctx) ->
    gen_attr(behaviour, V);
pp({attribute, _, Attr = author, V}, _Ctx) ->
    gen_attr(Attr, V);
pp({attribute, _, Attr = vsn, V}, _Ctx) ->
    gen_attr(Attr, V);
pp({attribute, _, Attr = date, V}, _Ctx) ->
    gen_attr(Attr, V);
pp({attribute, _, Attr = vc, V}, _Ctx) ->
    gen_attr(Attr, V);
pp({attribute, _, import, {ModNameAtom, Imports}}, Ctx) ->
    beside(text("import "),
           followc(Ctx,
                   besidel([text(":" ++ a2l(ModNameAtom)),
                            comma_f(),
                            text(" only:")]),
                   wrap_list(join(Imports,
                                  Ctx,
                                  fun pp_fn_import_ref/2,
                                  comma_f()))));
% TODO:
pp({attribute, _, export_type, _Exports}, _Ctx) ->
    % pp_attr_fun_list("@export_type ", Exports, Ctx);
    empty();
pp({attribute, _, on_load, V = {_FName, _Arity}}, Ctx) ->
    besidel([text("@on_load "), pp_fn_ref(V, Ctx)]);
pp({attribute, _, deprecated, _V = {_FName, _Arity, _When}}, _Ctx) ->
    %besidel([text("@deprecated "), pp_fn_deprecated_ref(V, Ctx)]);
    empty();
pp({attribute, _, deprecated, module}, _Ctx) ->
    text("@deprecated :module");
pp({attribute, _, deprecated, _Funs}, _Ctx) ->
    %besidel([text("@deprecated "),
    %         join(Funs, Ctx, fun pp_fn_deprecated_ref/2, comma_f())]);
    empty();
% TODO:
pp({attribute, _, deprecated_type, _}, _Ctx) ->
    empty();
% TODO:
pp({attribute, _, removed_type, _}, _Ctx) ->
    empty();
pp({attribute, _, inline, Exports}, Ctx) ->
    pp_attr_fun_list("@inline ", Exports, Ctx);
pp({attribute, _, export, _}, _Ctx) ->
    empty();
pp({attribute, _, compile, export_all}, _Ctx) ->
    text("@compile :export_all");
pp({attribute, _, compile, _}, _Ctx) ->
    empty();
pp({var, _, V}, #ctxt{stacktrace_varname = V}) ->
    text("__STACKTRACE__");
pp({var, _, V, _}, #ctxt{stacktrace_varname = V}) ->
    text("__STACKTRACE__");
pp({var, _, V, #{new := false, matching := true}}, #ctxt{}) ->
    text("^" ++ transform_var_name(V));
pp({var, _, V, _}, _Ctx) ->
    text(transform_var_name(V));
pp({var, _, V}, _Ctx) ->
    text(transform_var_name(V));
pp({atom, _, true}, _Ctx) ->
    text("true");
pp({atom, _, false}, _Ctx) ->
    text("false");
pp({atom, _, nil}, _Ctx) ->
    text("nil");
pp({atom, _, V}, _Ctx) ->
    quote_atom(V);
pp({integer, _, Num}, _Ctx) ->
    text(integer_to_list(Num));
pp({float, _, Num}, _Ctx) ->
    text(io_lib:write(Num));
pp({string, _, V}, _Ctx) ->
    text(io_lib:write_string(V, $'));
pp({bin, _, [{bin_element, _, {string, _, V}, default, default}]}, _Ctx) ->
    text(io_lib:write_string(V));
pp({char, _, $\s}, _Ctx) ->
    text("?\\s");
pp({char, _, $\r}, _Ctx) ->
    text("?\\r");
pp({char, _, $\t}, _Ctx) ->
    text("?\\t");
pp({char, _, $\n}, _Ctx) ->
    text("?\\n");
pp({char, _, $\f}, _Ctx) ->
    text("?\\f");
pp({char, _, $\e}, _Ctx) ->
    text("?\\e");
pp({char, _, $\d}, _Ctx) ->
    text("?\\d");
pp({char, _, $\b}, _Ctx) ->
    text("?\\b");
pp({char, _, $\v}, _Ctx) ->
    text("?\\v");
pp({char, _, $\a}, _Ctx) ->
    text("?\\a");
pp({char, _, V}, _Ctx) ->
    text("?" ++ [V]);
%% TODO: record
pp({record, _, RecName, Fields}, Ctx) ->
    pp_rec_new(RecName, Fields, Ctx);
pp({record, _, CurRecExpr, RecName, Fields}, Ctx) ->
    pp_rec_update(RecName, Fields, CurRecExpr, Ctx);
pp({record_field, _, RecExpr, RecName, Field}, Ctx) ->
    pp_rec_field(RecExpr, RecName, Field, Ctx);
pp({record_index, _, RecName, Field}, Ctx) ->
    pp_rec_index(RecName, Field, Ctx);
pp({bin, _, Elems}, Ctx) ->
    besidel([text("<<"), pp_bin_es(Elems, Ctx), text(">>")]);
% fun references
pp({'fun', Line, {function, FName, Arity}}, Ctx) ->
    beside(text("&"),
           wrap(text("/"),
                pp_call_pos({atom, Line, FName}, "", Ctx),
                pp({integer, Line, Arity}, Ctx)));
pp({'fun',
    _,
    {function,
     MName = {atom, _, _},
     FName = {atom, _, _},
     Arity = {integer, _, _}}},
   Ctx) ->
    beside(text("&"),
           wrap(text("/"),
                wrap(dot_f(),
                     pp_call_pos(MName, ":", Ctx),
                     pp_call_pos(FName, "", Ctx)),
                pp(Arity, Ctx)));
pp({'fun', _, {function, MName, FName, Arity}}, Ctx) ->
    besidel([text("Function.capture("),
             pp(MName, Ctx),
             text(", "),
             pp(FName, Ctx),
             text(", "),
             pp(Arity, Ctx),
             text(")")]);
pp({nil, _}, _Ctx) ->
    text("[]");
pp(V = {cons, _, _H, _T}, Ctx) ->
    pp_cons(V, Ctx);
pp({tuple, _, Items}, Ctx) ->
    wrap_tuple(pp_items(Items, Ctx));
pp({map, _, []}, _Ctx) ->
    text("%{}");
pp({map, _, Items}, Ctx) ->
    pp_map(Items, Ctx);
pp({map, _, CurMap, Items}, Ctx) ->
    wrap_map(sep([pp(CurMap, Ctx), pipe(), pp_map_inner(Items, Ctx)]));
pp({call, _, Expr={'fun', _, _}, Args}, Ctx) ->
    besidel([wrap_parens(pp(Expr, Ctx)), text("."), pp_args(Args, Ctx, fun pp/2)]);
pp({call, _, {remote, _, MName, FName}, Args}, Ctx) ->
    pp_call(MName, FName, Args, Ctx);
pp({call, _, FName, Args}, Ctx) ->
    pp_call(FName, Args, Ctx);
pp({'fun', _, {clauses, Clauses}}, Ctx) ->
    above(sep([text("fn"), nestc(Ctx, pp_case_clauses(Clauses, Ctx))]),
          text("end"));
pp({named_fun, _, AName, Clauses}, Ctx) ->
    above(sep([text("fn " ++ transform_var_name(AName)),
               pp_case_clauses(Clauses, Ctx)]),
          text("end"));
pp({function, _, Name, Arity, Clauses}, Ctx) ->
    IsExported = function_exported(Ctx, Name, Arity),
    DefKw =
        if IsExported ->
               "def";
           true ->
               "defp"
        end,
    % HACK: force a new line above each top level function
    pp_function_clauses(Clauses, Name, DefKw, Ctx);
pp({match, _, Left, Right}, Ctx) ->
    besidel([pp(Left, Ctx), text(" = "), pp(Right, Ctx)]);
pp({op, _, 'div', Left, Right}, Ctx) ->
    call_op("div(", Left, Right, Ctx);
pp({op, _, 'rem', Left, Right}, Ctx) ->
    call_op("rem(", Left, Right, Ctx);
pp({op, _, '!', Left, Right}, Ctx) ->
    call_op("send(", Left, Right, Ctx);
pp({op, Line, Op, Left, Right}, Ctx) ->
    case is_erlang_op(Op) of
        true ->
            op_to_erlang_call(Line, Op, [Left, Right], Ctx);
        false ->
            {LeftPrec, Prec, RightPrec} = inop_prec(Op),
            D1 = pp(Left, Ctx#ctxt{prec = LeftPrec}),
            D2 = text(atom_to_list(map_op_reverse(Op))),
            D3 = pp(Right, Ctx#ctxt{prec = RightPrec}),
            D4 = besidel([D1, text(" "), D2, text(" "), D3]),
            maybe_paren(Prec, Ctx#ctxt.prec, D4)
    end;
% unary
pp({op, Line, Op, Right}, Ctx) ->
    case is_erlang_op(Op) of
        true ->
            op_to_erlang_call(Line, Op, [Right], Ctx);
        false ->
            {Prec, RightPrec} = preop_prec(Op),
            LOp = text(atom_to_list(map_op_reverse(Op))),
            LRight = pp(Right, Ctx#ctxt{prec = RightPrec}),
            L = sep([LOp, LRight]),
            maybe_paren(Prec, Ctx#ctxt.prec, L)
    end;
pp({lc, _, Body, Gens}, Ctx) ->
    Ctx1 = reset_prec(Ctx),
    pp_for(Gens, Ctx1, pp(Body, Ctx1));
% https://elixir-lang.org/getting-started/comprehensions.html#bitstring-generators
% http://www.arh68.com/2016/01/17/drinking-more-elixir.html
pp({bc, _, Body, Gens}, Ctx) ->
    Ctx1 = reset_prec(Ctx),
    pp_bfor(Gens, Ctx1, pp(Body, Ctx1));
pp({block, _, Body}, Ctx) ->
    Ctx1 = reset_prec(Ctx),
    above(text("("), above(nestc(Ctx1, pp_body(Body, Ctx1)), text(")")));
pp({'if', _, Clauses}, Ctx) ->
    above(text("cond do"),
          above(nestc(Ctx, pp_if_clauses(Clauses, Ctx)), text("end")));
pp({'case', _, Expr, Clauses}, Ctx) ->
    % TODO: not always wrap Expr in paren, only nested statements
    above(besidel([text("case "), wrap_parens(pp(Expr, Ctx)), text(" do")]),
          above(nestc(Ctx, pp_case_clauses(Clauses, Ctx)), text("end")));
% receive no after
pp({'receive', _, Clauses}, Ctx0) ->
    Ctx = reset_prec(Ctx0),
    abovel([text("receive do"),
            nestc(Ctx, pp_case_clauses(Clauses, Ctx)),
            text("end")]);
pp({'receive', _, [], AfterExpr, AfterBody}, Ctx0) ->
    Ctx = reset_prec(Ctx0),
    abovel([besidel([text("receive do"),
                     text(" after "),
                     sep([pp(AfterExpr, Ctx), sarrow_f()])]),
            nestc(Ctx, pp_body(AfterBody, Ctx)),
            text("end")]);
pp({'receive', _, Clauses, AfterExpr, AfterBody}, Ctx0) ->
    Ctx = reset_prec(Ctx0),
    abovel([text("receive do"),
            nestc(Ctx, pp_case_clauses(Clauses, Ctx)),
            beside(text("after "), sep([pp(AfterExpr, Ctx), sarrow_f()])),
            nestc(Ctx, pp_body(AfterBody, Ctx)),
            text("end")]);
pp({'catch', _, Expr}, Ctx0) ->
    Ctx = reset_prec(Ctx0),
    above(text("(try do"),
          abovel([nestc(Ctx, pp(Expr, Ctx)),
                  text("catch"),
                  text("  :error, e -> {:EXIT, {e, __STACKTRACE__}}"),
                  text("  :exit, e -> {:EXIT, e}"),
                  text("  e -> e"),
                  text("end)")]));
pp({'try', _, Body, [], Clauses, AfterBody}, Ctx0) ->
    Ctx = reset_prec(Ctx0),
    above(text("try do"),
          above(maybe_above(maybe_above(nestc(Ctx, pp_body(Body, Ctx)),
                                        pp_try_catch_clauses(Clauses, Ctx)),
                            pp_try_after(AfterBody, Ctx)),
                text("end")));
pp({'try', _, Expr, OfCases, Clauses, AfterBody}, Ctx0) ->
    Ctx = reset_prec(Ctx0),
    abovel([abovel([text("try do"), nestc(Ctx, pp_body(Expr, Ctx))]),
            maybe_above(maybe_above(pp_try_catch_clauses(Clauses, Ctx),
                                    above(text("else"),
                                          nestc(Ctx,
                                                pp_case_clauses(OfCases,
                                                                Ctx)))),
                        pp_try_after(AfterBody, Ctx)),
            text("end")]);
pp({eof, _}, _Ctx) ->
    empty().

function_exported(#ctxt{exports_all = true}, _, _) ->
    true;
function_exported(#ctxt{exports = Exports}, Name, Arity) ->
    maps:is_key({Name, Arity}, Exports).

pp_function_clauses([Clause], Name, DefKw, Ctx) ->
    pp_function_clause(Clause, Name, DefKw, Ctx);
pp_function_clauses([Clause | Clauses], Name, DefKw, Ctx) ->
    above(pp_function_clause(Clause, Name, DefKw, Ctx),
          pp_function_clauses(Clauses, Name, DefKw, Ctx)).

pp_function_clause({clause, _, [], [], Body}, Name, DefKw, Ctx) ->
    pp_header_and_body(Ctx, text(DefKw ++ " " ++ a2l(Name) ++ " do"), Body);
pp_function_clause({clause, _, Patterns, [], Body}, Name, DefKw, Ctx) ->
    pp_header_and_body(Ctx,
                       beside(text(DefKw ++ " " ++ a2l(Name) ++ "("),
                              beside(pp_args_inn(Patterns, Ctx), text(") do"))),
                       Body);
pp_function_clause({clause, _, Patterns, Guards, Body}, Name, DefKw, Ctx) ->
    pp_header_and_body(Ctx,
                       followc(Ctx,
                               besidel([text(DefKw ++ " " ++ a2l(Name) ++ "("),
                                        pp_args_inn(Patterns, Ctx),
                                        text(")")]),
                               beside(text("when "),
                                      beside(pp_guards(Guards, Ctx),
                                             text(" do")))),
                       Body).

pp_args_inn(Args, Ctx) ->
    pp_args_inn(Args, Ctx, fun pp/2).

pp_args_inn([], _Ctx, _PPFun) ->
    empty();
pp_args_inn([Arg], Ctx, PPFun) ->
    PPFun(Arg, Ctx);
pp_args_inn(Args, Ctx, PPFun) ->
    join(Args, Ctx, PPFun, comma_f()).

pp_header_and_body_no_end(Ctx, HeaderLayout, Body) ->
    sep([HeaderLayout, nestc(Ctx, pp_body(Body, Ctx))]).

pp_header_and_body(Ctx, HeaderLayout, Body) ->
    above(pp_header_and_body_no_end(Ctx, HeaderLayout, Body), endk()).

pp_attr_fun_list(Prefix, Funs, Ctx) ->
    besidel([text(Prefix), join(Funs, Ctx, fun pp_fn_ref/2, comma_f())]).

gen_attr(Attr, V) when is_atom(V) ->
    text("@" ++ a2l(Attr) ++ " " ++ quote_atom_raw(V));
gen_attr(Attr, V) when is_list(V) ->
    text("@" ++ a2l(Attr) ++ " " ++ io_lib:write_string(V)).

quote_atom_raw(V) ->
    Chars = a2l(V),
    case re:run(Chars, "^[a-zA-Z_][a-zA-Z0-9@_]*$") of
        nomatch ->
            [":" | io_lib:write_string(Chars)];
        {match, _} ->
            [":" | Chars]
    end.

quote_record_field(V) ->
    Chars = a2l(V),
    % will quote all erlang reserved words, I think it's ok
    case io_lib:quote_atom(V, Chars) of
        true ->
            io_lib:write_string(Chars);
        false ->
            Chars
    end.

comma_f() ->
    floating(text(",")).

olist_f() ->
    floating(text("[")).

clist_f() ->
    floating(text("]")).

oparen_f() ->
    floating(text("(")).

cparen_f() ->
    floating(text(")")).

otuple_f() ->
    floating(text("{")).

ctuple_f() ->
    floating(text("}")).

omap_f() ->
    floating(text("%{")).

cmap_f() ->
    floating(text("}")).

arrow_f() ->
    text("=>").

sarrow_f() ->
    text("->").

dot_f() ->
    text(".").

dcolon_f() ->
    text("::").

endk() ->
    text("end\n").

pipe() ->
    text("|").

% not sure if the best way
pp_body([], _Ctx) ->
    empty();
pp_body([H | T], Ctx) ->
    above(pp(H, Ctx), pp_body(T, Ctx)).

abovel([]) ->
    empty();
abovel([H]) ->
    H;
% maybe skip empty() here?
abovel([H | T]) ->
    above(H, abovel(T)).

besidel([]) ->
    empty();
besidel([H]) ->
    H;
besidel([H | T]) ->
    beside(H, besidel(T)).

followc(Ctx, L1, L2) ->
    follow(L1, L2, Ctx#ctxt.sub_indent * 2).

parc(Ctx, L) ->
    par(L, Ctx#ctxt.sub_indent).

nestc(Ctx, Layout) ->
    nest(Ctx#ctxt.sub_indent, Layout).

% null is empty()
maybe_above(L, null) ->
    L;
maybe_above(null, L) ->
    L;
maybe_above(LLeft, LRight) ->
    above(LLeft, LRight).

join(Items, Ctx, PPFun, Sep) ->
    join(Items, Ctx, PPFun, Sep, []).

join([], _Ctx, _PPFun, _Sep, []) ->
    empty();
join([Item], Ctx, PPFun, _Sep, []) ->
    PPFun(Item, Ctx);
join([Item], Ctx, PPFun, _Sep, Accum) ->
    par(lists:reverse([PPFun(Item, Ctx) | Accum]), 2);
join([H | T = [_ | _]], Ctx, PPFun, Sep, Accum) ->
    join(T, Ctx, PPFun, Sep, [beside(PPFun(H, Ctx), Sep) | Accum]);
join([H | T], Ctx, PPFun, Sep, Accum) ->
    join([T], Ctx, PPFun, Sep, [beside(PPFun(H, Ctx), Sep) | Accum]).

wrap_list(Items) ->
    wrap(Items, olist_f(), clist_f()).

wrap(Items, Open, Close) ->
    beside(Open, beside(Items, Close)).

pp_fn_ref({FNameAtom, Arity}, _Ctx) ->
    text("&" ++ a2l(FNameAtom) ++ "/" ++ arity_to_list(Arity)).

pp_fn_import_ref({FNameAtom, Arity}, _Ctx) ->
    text("" ++ a2l(FNameAtom) ++ ": " ++ arity_to_list(Arity)).

pp_fn_deprecated_ref({FName, Arity, When}, _Ctx) when is_atom(When) ->
    text("(" ++
             a2l(FName) ++
                 ", " ++ arity_to_list(Arity) ++ ", " ++ a2l(When) ++ ")");
pp_fn_deprecated_ref({FName, Arity, Msg}, _Ctx) when is_list(Msg) ->
    text("(" ++
             a2l(FName) ++
                 ", " ++
                     arity_to_list(Arity) ++
                         ", " ++ io_lib:write_string(Msg) ++ ")");
pp_fn_deprecated_ref(FnRef, Ctx) ->
    pp_fn_ref(FnRef, Ctx).

arity_to_list('_') ->
    ":_";
arity_to_list(V) ->
    integer_to_list(V).

pp_call(FName, Args, Ctx) ->
    case should_prefix_erlang_call(FName, length(Args)) of
        true ->
            {_, Line, _} = FName,
            pp_call_f({atom, Line, erlang}, FName, Args, Ctx, fun pp/2);
        false ->
            pp_call_f(FName, Args, Ctx, fun pp/2)
    end.

pp_call(MName, FName, Args, Ctx) ->
    pp_call_f(MName, FName, Args, Ctx, fun pp/2).

pp_call_f(FName, Args, Ctx, PPFun) ->
    beside(pp_call_pos(FName, "", Ctx), pp_args(Args, Ctx, PPFun)).

pp_call_f(MName, FName = {var, Line, _}, Args, Ctx, _PPFun) ->
    pp_call_dyn_f(MName, FName, Args, Line, Ctx);
pp_call_f(MName, FName = {var, Line, _, _}, Args, Ctx, _PPFun) ->
    pp_call_dyn_f(MName, FName, Args, Line, Ctx);
pp_call_f(MName, FName, Args, Ctx, PPFun) ->
    beside(wrap(dot_f(),
                pp_call_method_pos(MName, ":", Ctx),
                pp_call_pos(FName, "", Ctx)),
           pp_args(Args, Ctx, PPFun)).

pp_call_dyn_f(MName, FName, Args, Line, Ctx) ->
    ApplyArgs = [MName, FName, list_to_cons(Args, Line)],
    pp({call, Line, {atom, Line, apply}, ApplyArgs}, Ctx).

pp_call_pos(V = {var, _, _}, _, Ctx) ->
    beside(pp(V, Ctx), text("."));
pp_call_pos(V = {var, _, _, _}, _, Ctx) ->
    beside(pp(V, Ctx), text("."));
pp_call_pos({atom, _, V}, Prefix, _Ctx) ->
    text(Prefix ++ a2l(V));
pp_call_pos(V, _, Ctx) ->
    beside(oparen_f(), beside(pp(V, Ctx), cparen_f())).

pp_call_method_pos(V = {var, _, _}, _, Ctx) ->
    pp(V, Ctx);
pp_call_method_pos(V = {var, _, _, _}, _, Ctx) ->
    pp(V, Ctx);
pp_call_method_pos({atom, _, V}, Prefix, _Ctx) ->
    text(Prefix ++ a2l(V));
pp_call_method_pos(V, _, Ctx) ->
    beside(oparen_f(), beside(pp(V, Ctx), cparen_f())).

pp_args([], _Ctx, _PPFun) ->
    text("()");
pp_args(Args, Ctx, PPFun) ->
    beside(oparen_f(), beside(pp_args_inn(Args, Ctx, PPFun), cparen_f())).

quote_atom(V) ->
    text(quote_atom_raw(V)).

pp_cons(V, Ctx) ->
    case cons_to_list(V, []) of
        [A | B] when not is_list(A), not is_list(B) ->
            wrap_list(followc(Ctx, beside(pp(A, Ctx), text(" |")), pp(B, Ctx)));
        [A | B] when not is_list(B) ->
            wrap_list(followc(Ctx,
                              beside(wrap_list(pp_items(A, Ctx)), text(" |")),
                              pp(B, Ctx)));
        L ->
            wrap_list(pp_items(L, Ctx))
    end.

pp_items(Items, Ctx) ->
    join(Items, Ctx, fun pp/2, comma_f()).

cons_to_list({cons, _, H, {nil, _}}, Accum) ->
    % proper list tail
    lists:reverse([H | Accum]);
cons_to_list({cons, _, H, T = {cons, _, _, _}}, Accum) ->
    cons_to_list(T, [H | Accum]);
cons_to_list({cons, _, H, T}, []) ->
    [H | T];
cons_to_list({cons, _, H, T}, Accum) ->
    % improper list
    [lists:reverse([H | Accum]) | T].

list_to_cons([], Line) ->
    {nil, Line};
list_to_cons([H | T], Line) ->
    {cons, Line, H, list_to_cons(T, Line)}.

wrap_tuple(Items) ->
    wrap(Items, otuple_f(), ctuple_f()).

wrap_map(Items) ->
    wrap(Items, omap_f(), cmap_f()).

wrap_parens(Items) ->
    wrap(Items, oparen_f(), cparen_f()).

pp_map(Items, Ctx) ->
    wrap_map(pp_map_inner(Items, Ctx)).

pp_map_inner(Items, Ctx) ->
    join(Items, Ctx, fun pp_pair/2, comma_f()).

pp_pair({map_field_assoc, _, K, V}, Ctx) ->
    wrap_pair(Ctx, arrow_f(), pp(K, Ctx), pp(V, Ctx));
%pp_pair({map_field_exact, _, {atom, _, K}, V}, Ctx) ->
%    wrap_pair_no_left_space(Ctx, colon_f(), text(a2l(K)), pp(V, Ctx));
pp_pair({map_field_exact, _, K, V}, Ctx) ->
    wrap_pair(Ctx, arrow_f(), pp(K, Ctx), pp(V, Ctx)).

wrap_pair(Ctx, Sep, Left, Right) ->
    parc(Ctx, [sep([Left, Sep, Right])]).

%wrap_pair_no_left_space(Ctx, Sep, Left, Right) ->
%    parc(Ctx, [beside(Left, Sep), Right]).

maybe_paren(P, Prec, Expr) when P < Prec ->
    wrap_paren(Expr);
maybe_paren(_P, _Prec, Expr) ->
    Expr.

wrap_paren(Expr) ->
    beside(beside(oparen_f(), Expr), cparen_f()).

pp_if_clauses([Clause], Ctx) ->
    pp_if_clause(Clause, Ctx);
pp_if_clauses([Clause | Clauses = [_ | _]], Ctx) ->
    above(pp_if_clause(Clause, Ctx), pp_if_clauses(Clauses, Ctx)).

pp_if_clause({clause, _, _, Guards, Body}, Ctx) ->
    pp_header_and_body_no_end(Ctx, pp_if_header(Ctx, "", Guards), Body).

pp_if_header(Ctx, KwT, Guards) ->
    besidel([text(KwT), pp_guards(Guards, Ctx), text(" ->")]).

pp_case_clauses([Clause], Ctx) ->
    pp_case_clause(Clause, Ctx);
pp_case_clauses([Clause | Clauses], Ctx) ->
    above(pp_case_clause(Clause, Ctx), pp_case_clauses(Clauses, Ctx)).

pp_case_clause({clause, _, [], [], Body}, Ctx) ->
    pp_header_and_body_no_end(Ctx, text(" ->"), Body);
pp_case_clause({clause, _, Patterns, [], Body}, Ctx) ->
    pp_header_and_body_no_end(Ctx,
                              besidel([pp_args_inn(Patterns, Ctx), text(" ->")]),
                              Body);
pp_case_clause({clause, _, Patterns, Guards, Body}, Ctx) ->
    pp_header_and_body_no_end(Ctx,
                              followc(Ctx,
                                      pp_args_inn(Patterns, Ctx),
                                      beside(text("when "),
                                             sep([pp_guards(Guards, Ctx),
                                                  sarrow_f()]))),
                              Body).

pp_try_catch_clauses([], _Ctx) ->
    empty();
pp_try_catch_clauses(Clauses, Ctx) ->
    above(text("catch"), nestc(Ctx, pp_try_catch_cases(Clauses, Ctx))).

pp_try_after([], _Ctx) ->
    empty();
pp_try_after(Body, Ctx) ->
    above(text("after"), nestc(Ctx, pp_body(Body, Ctx))).

pp_try_catch_cases([], _Ctx) ->
    empty();
pp_try_catch_cases([H | T], Ctx) ->
    above(pp_try_catch_case(H, Ctx), pp_try_catch_cases(T, Ctx)).

pp_try_catch_case({clause, _, [{tuple, _, TItems}], [], Body}, Ctx) ->
    Ctx1 = maybe_set_catch_stacktrace_var(Ctx, TItems),
    pp_header_and_body_no_end(Ctx1,
                              sep([pp_try_catch_case_items(TItems, Ctx),
                                   sarrow_f()]),
                              Body);
pp_try_catch_case({clause, _, [{tuple, _, TItems}], Guards, Body}, Ctx) ->
    Ctx1 = maybe_set_catch_stacktrace_var(Ctx, TItems),
    pp_header_and_body_no_end(Ctx1,
                              followc(Ctx,
                                      pp_try_catch_case_items(TItems, Ctx),
                                      beside(text("when "),
                                             sep([pp_guards(Guards, Ctx),
                                                  sarrow_f()]))),
                              Body).

pp_try_catch_case_items([{atom, _, throw}, Var, _], Ctx) ->
    pp(Var, Ctx);
pp_try_catch_case_items([Type, Var, _], Ctx) ->
    pp_items([Type, Var], Ctx).

maybe_set_catch_stacktrace_var(Ctx, [_, _, {var, _, Name}]) ->
    set_stacktrace_var(Ctx, Name);
maybe_set_catch_stacktrace_var(Ctx, [_, _, {var, _, Name, _}]) ->
    set_stacktrace_var(Ctx, Name).

pp_for(Gens, Ctx, BodyL) ->
    above(besidel([text("for "), pp_lc_gens(Gens, Ctx), text(" do")]),
          above(nestc(Ctx, BodyL), text("end"))).

pp_bfor(Gens, Ctx, BodyL) ->
    above(sep([text("for"), pp_lc_gens(Gens, Ctx), text(", into: <<>> do")]),
          above(nestc(Ctx, BodyL), text("end"))).

pp_lc_gens(Items, Ctx) ->
    join(Items, Ctx, fun pp_lc_gen/2, comma_f()).

pp_lc_gen({generate, _, Left, Right}, Ctx) ->
    % TODO: add parens only when statement
    wrap(text(" <- "), pp(Left, Ctx), wrap_parens(pp(Right, Ctx)));
pp_lc_gen({b_generate, _, Left, Right}, Ctx) ->
    besidel([text("<< "),
             wrap(text(" <- "), pp(Left, Ctx), pp(Right, Ctx)),
             text(" >>")]);
pp_lc_gen(Filter, Ctx) ->
    pp(Filter, Ctx).

% TODO: precedence
pp_guards(Guards, Ctx) ->
    join(Guards, Ctx, fun pp_guard/2, text(" or")).

% TODO: precedence
pp_guard(SGuards, Ctx) ->
    join(SGuards, Ctx, fun pp/2, text(" and")).

pp_bin_es(Es, Ctx) ->
    join(Es, Ctx, fun pp_bin_e/2, comma_f()).

pp_bin_e({bin_element, _, Left, default, [binary]}, Ctx) ->
    wrap_pair(Ctx, dcolon_f(), pp_bin_e_v(Left, Ctx), text("binary"));
pp_bin_e({bin_element, _, Left, Size, default}, Ctx) when Size =/= default ->
    wrap_pair(Ctx, dcolon_f(), pp_bin_e_v(Left, Ctx), pp(Size, Ctx));
pp_bin_e({bin_element, _, Left, default, default}, Ctx) ->
    pp_bin_e_v(Left, Ctx);
pp_bin_e({bin_element, _, Left, Size, Types}, Ctx) ->
    TypeMap = pp_bin_e_types(Types, Size, Ctx),
    wrap_pair(Ctx, dcolon_f(), pp(Left, Ctx), TypeMap).

pp_bin_e_v({string, _, V}, _Ctx) ->
    text(io_lib:write_string(V));
pp_bin_e_v(V, Ctx) ->
    pp(V, Ctx).

pp_bin_e_types(Types, default, Ctx) ->
    pp_bin_e_types(Types, Ctx);
pp_bin_e_types(Types, Size, Ctx) ->
    pp_bin_e_types([{size, Size} | Types], Ctx).

pp_bin_e_types(Types, Ctx) ->
    join(Types, Ctx, fun pp_bin_e_type/2, text(" -")).

pp_attr_pair(_Ctx, _KeyTxt, ValTxt) ->
    text(ValTxt).

pp_bin_e_type({size, default}, _Ctx) ->
    empty();
pp_bin_e_type({size, V}, Ctx) ->
    besidel([text("size("), pp(V, Ctx), text(")")]);
pp_bin_e_type({unit, V}, _Ctx) ->
    besidel([text("unit("), text(integer_to_list(V)), text(")")]);
pp_bin_e_type(Type = integer, Ctx) ->
    pp_attr_pair(Ctx, "type", atom_to_list(Type));
pp_bin_e_type(Type = float, Ctx) ->
    pp_attr_pair(Ctx, "type", atom_to_list(Type));
pp_bin_e_type(Type = binary, Ctx) ->
    pp_attr_pair(Ctx, "type", atom_to_list(Type));
pp_bin_e_type(Type = bytes, Ctx) ->
    pp_attr_pair(Ctx, "type", atom_to_list(Type));
pp_bin_e_type(Type = bitstring, Ctx) ->
    pp_attr_pair(Ctx, "type", atom_to_list(Type));
pp_bin_e_type(Type = bits, Ctx) ->
    pp_attr_pair(Ctx, "type", atom_to_list(Type));
pp_bin_e_type(Type = utf8, Ctx) ->
    pp_attr_pair(Ctx, "type", atom_to_list(Type));
pp_bin_e_type(Type = utf16, Ctx) ->
    pp_attr_pair(Ctx, "type", atom_to_list(Type));
pp_bin_e_type(Type = utf32, Ctx) ->
    pp_attr_pair(Ctx, "type", atom_to_list(Type));
pp_bin_e_type(Type = signed, Ctx) ->
    pp_attr_pair(Ctx, "sign", atom_to_list(Type));
pp_bin_e_type(Type = unsigned, Ctx) ->
    pp_attr_pair(Ctx, "sign", atom_to_list(Type));
pp_bin_e_type(Type = big, Ctx) ->
    pp_attr_pair(Ctx, "endianness", atom_to_list(Type));
pp_bin_e_type(Type = little, Ctx) ->
    pp_attr_pair(Ctx, "endianness", atom_to_list(Type));
pp_bin_e_type(Type = native, Ctx) ->
    pp_attr_pair(Ctx, "endianness", atom_to_list(Type)).

call_op(OpenText, Left, Right, Ctx) ->
    besidel([text(OpenText),
             pp(Left, Ctx),
             text(", "),
             pp(Right, Ctx),
             text(")")]).

add_record_declaration(RecName, Fields, Ctx = #ctxt{records = Records}) ->
    FieldInfo =
        [parse_record_field(Field, Pos) || {Pos, Field} <- enumerate(Fields)],
    RecInfo = #{fields => maps:from_list(FieldInfo), field_order => FieldInfo},
    NewRecords = Records#{RecName => RecInfo},
    Ctx#ctxt{records = NewRecords}.

enumerate(Items) ->
    enumerate(Items, [], 0).

enumerate([], Accum, _) ->
    lists:reverse(Accum);
enumerate([H | T], Accum, N) ->
    enumerate(T, [{N, H} | Accum], N + 1).

pp_record_field_decl({typed_record_field, Field, _Type}, Ctx) ->
    pp_record_field_decl(Field, Ctx);
pp_record_field_decl({record_field, L1, {atom, L2, Name}}, Ctx) ->
    pp_record_field_decl({record_field,
                          L1,
                          {atom, L2, Name},
                          {atom, L2, undefined}},
                         Ctx);
pp_record_field_decl({record_field, _, {atom, _, Name}, {record, _, _, _}}, _Ctx) ->
    besidel([text(quote_record_field(Name)), text(": :TODO_NESTED_RECORD")]);
pp_record_field_decl({record_field, _, {atom, _, Name}, Default}, Ctx) ->
    besidel([text(quote_record_field(Name)), text(": "), pp(Default, Ctx)]).

parse_record_field({typed_record_field, Field, _Type}, Pos) ->
    parse_record_field(Field, Pos);
parse_record_field({record_field, _, {atom, _, Name}}, Pos) ->
    {Name, #{default => {atom, 0, undefined}, position => Pos + 1}};
parse_record_field({record_field, _, {atom, _, Name}, Default}, Pos) ->
    {Name, #{default => Default, position => Pos + 1}}.

pp_rec_new(RecName, Fields, Ctx = #ctxt{}) ->
    besidel([p_rec_name(RecName),
             text("("),
             join(Fields, Ctx, fun pp_rec_update_field/2, comma_f()),
             text(")")]).

pp_rec_update(RecName, [], CurRecExpr, Ctx = #ctxt{}) ->
    % zero fields record update O.o
    besidel([p_rec_name(RecName), text("("), pp(CurRecExpr, Ctx), text(")")]);
pp_rec_update(RecName, Fields, CurRecExpr, Ctx = #ctxt{}) ->
    besidel([p_rec_name(RecName),
             text("("),
             pp(CurRecExpr, Ctx),
             text(", "),
             join(Fields, Ctx, fun pp_rec_update_field/2, text(", ")),
             text(")")]).

pp_rec_field(RecExpr, RecName, {atom, _, Field}, Ctx = #ctxt{}) ->
    besidel([p_rec_name(RecName),
             text("("),
             pp(RecExpr, Ctx),
             text(", :"),
             text(quote_record_field(Field)),
             text(")")]).

pp_rec_index(RecName, {atom, _, Field}, #ctxt{}) ->
    besidel([p_rec_name(RecName),
             text("("),
             text(":" ++ atom_to_list(Field)),
             text(")")]).

pp_rec_update_field({record_field, _, {var, _, '_'}, Value}, Ctx) ->
    besidel([text("_"), text(": "), pp(Value, Ctx)]);
pp_rec_update_field({record_field, _, {atom, _, FieldName}, Value}, Ctx) ->
    besidel([text(quote_record_field(FieldName)), text(": "), pp(Value, Ctx)]).

% TODO: if bit ops are used Bitwise must be included: https://hexdocs.pm/elixir/Bitwise.html
map_op_reverse('rem') ->
    'rem';
map_op_reverse('div') ->
    'div';
map_op_reverse('=') ->
    '=';
map_op_reverse('+') ->
    '+';
map_op_reverse('-') ->
    '-';
map_op_reverse('*') ->
    '*';
map_op_reverse('/') ->
    '/';
map_op_reverse('bor') ->
    '|||';
map_op_reverse('band') ->
    '&&&';
map_op_reverse('bxor') ->
    '^^^';
map_op_reverse('bsr') ->
    '>>>';
map_op_reverse('bsl') ->
    '<<<';
map_op_reverse('bnot') ->
    '~~~';
map_op_reverse('andalso') ->
    'and';
map_op_reverse('orelse') ->
    'or';
map_op_reverse('xor') ->
    'xor';
map_op_reverse('!') ->
    '!';
map_op_reverse('not') ->
    'not';
map_op_reverse('++') ->
    '++';
map_op_reverse('--') ->
    '--';
map_op_reverse('<') ->
    '<';
map_op_reverse('=<') ->
    '<=';
map_op_reverse('>') ->
    '>';
map_op_reverse('>=') ->
    '>=';
map_op_reverse('==') ->
    '==';
map_op_reverse('=:=') ->
    '===';
map_op_reverse('/=') ->
    '!=';
map_op_reverse('=/=') ->
    '!=='.

is_erlang_op('and') ->
    true;
is_erlang_op('or') ->
    true;
is_erlang_op('xor') ->
    true;
is_erlang_op('bor') ->
    true;
is_erlang_op('band') ->
    true;
is_erlang_op('bxor') ->
    true;
is_erlang_op('bsr') ->
    true;
is_erlang_op('bsl') ->
    true;
is_erlang_op('bnot') ->
    true;
is_erlang_op(_) ->
    false.

is_autoimported(abs, 1) ->
    true;
is_autoimported(apply, 2) ->
    true;
is_autoimported(apply, 3) ->
    true;
is_autoimported(atom_to_binary, 2) ->
    true;
is_autoimported(atom_to_list, 1) ->
    true;
is_autoimported(binary_part, 2) ->
    true;
is_autoimported(binary_part, 3) ->
    true;
is_autoimported(binary_to_atom, 2) ->
    true;
is_autoimported(binary_to_existing_atom, 2) ->
    true;
is_autoimported(binary_to_float, 1) ->
    true;
is_autoimported(binary_to_integer, 1) ->
    true;
is_autoimported(binary_to_integer, 2) ->
    true;
is_autoimported(binary_to_list, 1) ->
    true;
is_autoimported(binary_to_list, 3) ->
    true;
is_autoimported(binary_to_term, 1) ->
    true;
is_autoimported(binary_to_term, 2) ->
    true;
is_autoimported(bit_size, 1) ->
    true;
is_autoimported(bitstring_to_list, 1) ->
    true;
is_autoimported(byte_size, 1) ->
    true;
is_autoimported(ceil, 1) ->
    true;
is_autoimported(check_old_code, 1) ->
    true;
is_autoimported(check_process_code, 2) ->
    true;
is_autoimported(check_process_code, 3) ->
    true;
is_autoimported(date, 0) ->
    true;
is_autoimported(delete_module, 1) ->
    true;
is_autoimported(demonitor, 1) ->
    true;
is_autoimported(demonitor, 2) ->
    true;
is_autoimported(disconnect_node, 1) ->
    true;
is_autoimported(element, 2) ->
    true;
is_autoimported(erase, 0) ->
    true;
is_autoimported(erase, 1) ->
    true;
is_autoimported(error, 1) ->
    true;
is_autoimported(error, 2) ->
    true;
is_autoimported(exit, 1) ->
    true;
is_autoimported(exit, 2) ->
    true;
is_autoimported(float, 1) ->
    true;
is_autoimported(float_to_binary, 1) ->
    true;
is_autoimported(float_to_binary, 2) ->
    true;
is_autoimported(float_to_list, 1) ->
    true;
is_autoimported(float_to_list, 2) ->
    true;
is_autoimported(floor, 1) ->
    true;
is_autoimported(garbage_collect, 0) ->
    true;
is_autoimported(garbage_collect, 1) ->
    true;
is_autoimported(garbage_collect, 2) ->
    true;
is_autoimported(get, 0) ->
    true;
is_autoimported(get, 1) ->
    true;
is_autoimported(get_keys, 0) ->
    true;
is_autoimported(get_keys, 1) ->
    true;
is_autoimported(group_leader, 0) ->
    true;
is_autoimported(group_leader, 2) ->
    true;
is_autoimported(halt, 0) ->
    true;
is_autoimported(halt, 1) ->
    true;
is_autoimported(halt, 2) ->
    true;
is_autoimported(hd, 1) ->
    true;
is_autoimported(integer_to_binary, 1) ->
    true;
is_autoimported(integer_to_binary, 2) ->
    true;
is_autoimported(integer_to_list, 1) ->
    true;
is_autoimported(integer_to_list, 2) ->
    true;
is_autoimported(iolist_size, 1) ->
    true;
is_autoimported(iolist_to_binary, 1) ->
    true;
is_autoimported(is_alive, 0) ->
    true;
is_autoimported(is_atom, 1) ->
    true;
is_autoimported(is_binary, 1) ->
    true;
is_autoimported(is_bitstring, 1) ->
    true;
is_autoimported(is_boolean, 1) ->
    true;
is_autoimported(is_float, 1) ->
    true;
is_autoimported(is_function, 1) ->
    true;
is_autoimported(is_function, 2) ->
    true;
is_autoimported(is_integer, 1) ->
    true;
is_autoimported(is_list, 1) ->
    true;
is_autoimported(is_map, 1) ->
    true;
is_autoimported(is_map_key, 2) ->
    true;
is_autoimported(is_number, 1) ->
    true;
is_autoimported(is_pid, 1) ->
    true;
is_autoimported(is_port, 1) ->
    true;
is_autoimported(is_process_alive, 1) ->
    true;
is_autoimported(is_record, 2) ->
    true;
is_autoimported(is_record, 3) ->
    true;
is_autoimported(is_reference, 1) ->
    true;
is_autoimported(is_tuple, 1) ->
    true;
is_autoimported(length, 1) ->
    true;
is_autoimported(link, 1) ->
    true;
is_autoimported(list_to_atom, 1) ->
    true;
is_autoimported(list_to_binary, 1) ->
    true;
is_autoimported(list_to_bitstring, 1) ->
    true;
is_autoimported(list_to_existing_atom, 1) ->
    true;
is_autoimported(list_to_float, 1) ->
    true;
is_autoimported(list_to_integer, 1) ->
    true;
is_autoimported(list_to_integer, 2) ->
    true;
is_autoimported(list_to_pid, 1) ->
    true;
is_autoimported(list_to_port, 1) ->
    true;
is_autoimported(list_to_ref, 1) ->
    true;
is_autoimported(list_to_tuple, 1) ->
    true;
is_autoimported(load_module, 2) ->
    true;
is_autoimported(make_ref, 0) ->
    true;
is_autoimported(map_get, 2) ->
    true;
is_autoimported(map_size, 1) ->
    true;
is_autoimported(max, 2) ->
    true;
is_autoimported(min, 2) ->
    true;
is_autoimported(module_loaded, 1) ->
    true;
is_autoimported(monitor, 2) ->
    true;
is_autoimported(node, 0) ->
    true;
is_autoimported(node, 1) ->
    true;
is_autoimported(nodes, 0) ->
    true;
is_autoimported(nodes, 1) ->
    true;
is_autoimported(now, 0) ->
    true;
is_autoimported(open_port, 2) ->
    true;
is_autoimported(pid_to_list, 1) ->
    true;
is_autoimported(port_close, 1) ->
    true;
is_autoimported(port_command, 2) ->
    true;
is_autoimported(port_command, 3) ->
    true;
is_autoimported(port_connect, 2) ->
    true;
is_autoimported(port_control, 3) ->
    true;
is_autoimported(port_to_list, 1) ->
    true;
is_autoimported(pre_loaded, 0) ->
    true;
is_autoimported(process_flag, 2) ->
    true;
is_autoimported(process_flag, 3) ->
    true;
is_autoimported(process_info, 1) ->
    true;
is_autoimported(process_info, 2) ->
    true;
is_autoimported(processes, 0) ->
    true;
is_autoimported(purge_module, 1) ->
    true;
is_autoimported(put, 2) ->
    true;
is_autoimported(ref_to_list, 1) ->
    true;
is_autoimported(register, 2) ->
    true;
is_autoimported(registered, 0) ->
    true;
is_autoimported(round, 1) ->
    true;
is_autoimported(self, 0) ->
    true;
is_autoimported(setelement, 3) ->
    true;
is_autoimported(size, 1) ->
    true;
is_autoimported(spawn, 1) ->
    true;
is_autoimported(spawn, 2) ->
    true;
is_autoimported(spawn, 3) ->
    true;
is_autoimported(spawn, 4) ->
    true;
is_autoimported(spawn_link, 1) ->
    true;
is_autoimported(spawn_link, 2) ->
    true;
is_autoimported(spawn_link, 3) ->
    true;
is_autoimported(spawn_link, 4) ->
    true;
is_autoimported(spawn_monitor, 1) ->
    true;
is_autoimported(spawn_monitor, 3) ->
    true;
is_autoimported(spawn_opt, 2) ->
    true;
is_autoimported(spawn_opt, 3) ->
    true;
is_autoimported(spawn_opt, 4) ->
    true;
is_autoimported(spawn_opt, 5) ->
    true;
is_autoimported(split_binary, 2) ->
    true;
is_autoimported(term_to_binary, 1) ->
    true;
is_autoimported(term_to_binary, 2) ->
    true;
is_autoimported(throw, 1) ->
    true;
is_autoimported(time, 0) ->
    true;
is_autoimported(tl, 1) ->
    true;
is_autoimported(trunc, 1) ->
    true;
is_autoimported(tuple_size, 1) ->
    true;
is_autoimported(tuple_to_list, 1) ->
    true;
is_autoimported(unlink, 1) ->
    true;
is_autoimported(unregister, 1) ->
    true;
is_autoimported(whereis, 1) ->
    true;
is_autoimported(_, _) ->
    false.

is_ex_autoimport(abs, 1) ->
    true;
is_ex_autoimport(apply, 2) ->
    true;
is_ex_autoimport(apply, 3) ->
    true;
is_ex_autoimport(binary_part, 3) ->
    true;
is_ex_autoimport(bit_size, 1) ->
    true;
is_ex_autoimport(byte_size, 1) ->
    true;
is_ex_autoimport(ceil, 1) ->
    true;
is_ex_autoimport(exit, 1) ->
    true;
is_ex_autoimport(floor, 1) ->
    true;
is_ex_autoimport(hd, 1) ->
    true;
is_ex_autoimport(is_atom, 1) ->
    true;
is_ex_autoimport(is_binary, 1) ->
    true;
is_ex_autoimport(is_bitstring, 1) ->
    true;
is_ex_autoimport(is_boolean, 1) ->
    true;
is_ex_autoimport(is_float, 1) ->
    true;
is_ex_autoimport(is_function, 1) ->
    true;
is_ex_autoimport(is_function, 2) ->
    true;
is_ex_autoimport(is_integer, 1) ->
    true;
is_ex_autoimport(is_list, 1) ->
    true;
is_ex_autoimport(is_map, 1) ->
    true;
is_ex_autoimport(is_number, 1) ->
    true;
is_ex_autoimport(is_pid, 1) ->
    true;
is_ex_autoimport(is_port, 1) ->
    true;
is_ex_autoimport(is_reference, 1) ->
    true;
is_ex_autoimport(is_tuple, 1) ->
    true;
is_ex_autoimport(length, 1) ->
    true;
is_ex_autoimport(make_ref, 0) ->
    true;
is_ex_autoimport(map_size, 1) ->
    true;
is_ex_autoimport(max, 2) ->
    true;
is_ex_autoimport(min, 2) ->
    true;
is_ex_autoimport(node, 0) ->
    true;
is_ex_autoimport(node, 1) ->
    true;
is_ex_autoimport(round, 1) ->
    true;
is_ex_autoimport(self, 0) ->
    true;
is_ex_autoimport(spawn, 1) ->
    true;
is_ex_autoimport(spawn, 3) ->
    true;
is_ex_autoimport(spawn_link, 1) ->
    true;
is_ex_autoimport(spawn_link, 3) ->
    true;
is_ex_autoimport(spawn_monitor, 1) ->
    true;
is_ex_autoimport(spawn_monitor, 3) ->
    true;
is_ex_autoimport(throw, 1) ->
    true;
is_ex_autoimport(tl, 1) ->
    true;
is_ex_autoimport(trunc, 1) ->
    true;
is_ex_autoimport(tuple_size, 1) ->
    true;
is_ex_autoimport(_, _) ->
    false.

should_prefix_erlang_call({atom, _, FName}, Arity) ->
    should_prefix_erlang_call(FName, Arity);
should_prefix_erlang_call({_, _, _}, _Arity) ->
    false;
should_prefix_erlang_call({var, _, _, _}, _Arity) ->
    false;
should_prefix_erlang_call(FName, Arity) when is_atom(FName), is_number(Arity) ->
    is_autoimported(FName, Arity) andalso not is_ex_autoimport(FName, Arity);
% stuff like {call,826,{atom,826,predef_fun},[]}
should_prefix_erlang_call({call, _, _, _}, _Arity) ->
    false;
should_prefix_erlang_call({record_field, _, _RecExpr, _RecName, _Field},
                          _Arity) ->
    false.

is_ex_reserved_varname("nil") ->
    true;
is_ex_reserved_varname("true") ->
    true;
is_ex_reserved_varname("false") ->
    true;
is_ex_reserved_varname("def") ->
    true;
is_ex_reserved_varname("if") ->
    true;
is_ex_reserved_varname("unless") ->
    true;
is_ex_reserved_varname("cond") ->
    true;
is_ex_reserved_varname("case") ->
    true;
is_ex_reserved_varname("when") ->
    true;
is_ex_reserved_varname("and") ->
    true;
is_ex_reserved_varname("or") ->
    true;
is_ex_reserved_varname("not") ->
    true;
is_ex_reserved_varname("in") ->
    true;
is_ex_reserved_varname("fn") ->
    true;
is_ex_reserved_varname("do") ->
    true;
is_ex_reserved_varname("end") ->
    true;
is_ex_reserved_varname("catch") ->
    true;
is_ex_reserved_varname("rescue") ->
    true;
is_ex_reserved_varname("after") ->
    true;
is_ex_reserved_varname("else") ->
    true;
is_ex_reserved_varname(_) ->
    false.

a2l(V) ->
    atom_to_list(V).

transform_var_name(V) ->
    case a2l(V) of
        L = [$_ | _] ->
            L;
        [H] ->
            string:lowercase([H]);
        [H | T] ->
            VarName = string:lowercase([H]) ++ T,
            case is_ex_reserved_varname(VarName) of
                true ->
                    VarName ++ "__";
                false ->
                    VarName
            end
    end.

op_to_erlang_call(Line, Op, Args, Ctx) ->
    pp({call,
        Line,
        {remote, Line, {atom, Line, erlang}, {atom, Line, Op}},
        Args},
       Ctx).

p_rec_name(RecName) ->
    Name = a2l(RecName),
    text(["r_" | Name]).
