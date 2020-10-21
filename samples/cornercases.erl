-module(cornercases).

-export([varnames/12, atoms/0, chars/0, calls/3, calls/0, long_calc/0, binops/0,
         instant_call/0, lc_no_gen/0, module_macro/0, uppercase_funs/2,
         fun_no_args_when/1, 'substring-after'/0, call_call/0,
         escape_str_interpolation/0, printable_chars/0, wrap_stmt/1,
         local_calls_qualified/2, '+~+'/0, alias/2, def/2, call_conflict/0,
         in/3, encoding/0, module_info_calls/0, or_in_guard/1,
         guard_presedence/1, macros/0, template_cols/1]).

% on_load is private
-on_load on_load/0.

% duplicated import
-import(foo, [to_string/1, to_string/1]).

send(A, B) ->
    A ! B.

local_calls_qualified(A, B) ->
    % should add module to disambiguate from Kernel.* versions
    send(self(), ok),
    {to_string(42), monitor_node(A, B)}.

on_load() ->
    ok.

varnames(When, And, Or, Not, In, Fn, Do, End, Catch, Rescue, After, Else) ->
    {When, And, Or, Not, In, Fn, Do, End, Catch, Rescue, After, Else}.

atoms() ->
    ['type-id'].

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
    Enc = utf8,
    Opts = [],
    % elixir for needs a generator, add a dummy `_ <- [nil]` generator
    {[ok || true],
     [ok || false],
     [ok || _ <- [nil], true],
     [ok || _ <- [nil], false],
     [{encoding, Enc} || Enc =/= none, not proplists:get_bool(comments, Opts)]}.

module_macro() ->
    ?MODULE.

uppercase_funs(A, B) ->
    wxMenu:'Destroy'(A, B).

fun_no_args_when(A) ->
    {fun () when A =:= 10 ->
             ok;
         () ->
             error
     end,
     fun () ->
             ok
     end,
     fun (B) ->
             B
     end}.

% xerl_xpath_pred
'substring-after'() ->
    ok.

return_fn() ->
    fun () ->
            ok
    end.

call_call() ->
    (return_fn())().

escape_str_interpolation() ->
    ["#{", '#{', "'p'"].

chars() ->
    [$\s, $\t, $\r, $\n, $\f, $\e, $\d, $\b, $\v, $\^G, $\^C].

printable_chars() ->
    [$a, $z, $A, $Z, $0, $9, $\000, $\377, $\\, $\n].

wrap_stmt(Cs) ->
    [C
     || C <- Cs,
        lists:member(C, [ed25519, ed448, x25519, x448]) orelse
            try crypto:generate_key(ecdh, C) of
                _ ->
                    true
            catch
                _:_ ->
                    false
            end].

'+~+'() ->
    '+~+'().

alias(A, B) ->
    {A, B}.

def(A, B) ->
    {A, B}.

call_conflict() ->
    alias(1, 2),
    def(1, 2).

in(A, B, C) ->
    A + B + C.

encoding() ->
    {"&ouml;", "ö"},
    {"&auml;", "ä"},
    {"&aring;", "å"}.

module_info_calls() ->
    {module_info(), module_info(md5)}.

or_in_guard(A) when (A == 8) or (A == 16) and (A == 18) ->
    ok.

guard_presedence(A) when A == 1, A == 2; A == 3 ->
    ok.

macros() ->
    {?VSN, ?COMPILER_VSN}.

template_cols(ColumnClasses) ->
    lists:sort([{{IdNo, Col}, lists:usort(Cs)}
                || Class <- ColumnClasses,
                   {IdNo, Col} <- Class,
                   IdNo =/= 1,
                   [] =/= (Cs = [C || {1, C} <- Class])]).
