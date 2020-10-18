-module(efe).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(["pp", Path]) ->
    pprint_ex(Path, true);
main(["ppe", Path]) ->
    pprint_ex(Path, false);
main(["ann", Path]) ->
    {Ast, _St} = annotate(Path),
    pprint({ok, Ast});
main(_) ->
    io:format("Usage: efe pp <path.erl>"),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

pprint({ok, R}) ->
    io:format("~p~n", [R]);
pprint({error, E}) ->
    io:format("Error: ~p~n", [E]).

with_ast(Path, Fn) ->
    case from_erl(Path) of
        {ok, Ast} ->
            try
                Fn(Ast)
            catch
                T:E:S ->
                    {error,
                     #{code => exception, type => T, error => E, stack => S}}
            end;
        Other ->
            Other
    end.

annotate(Path) ->
    with_ast(Path,
             fun (Ast) ->
                     efe_var_ann:do(Ast)
             end).

from_erl(Path) ->
    PathDir = filename:dirname(Path),
    IncludePaths =
        filelib:wildcard(filename:join([PathDir, "../../*/include"])),
    epp:parse_file(Path, [PathDir | IncludePaths], []).

pprint_ex(Path, DoPrint) ->
    case from_erl(Path) of
        {ok, Ast} ->
            try
                {AnnAst, _St} = efe_var_ann:do(Ast),
                case DoPrint of
                    true ->
                        io:format("~s~n", [efe_pp:format(AnnAst)]);
                    false ->
                        ok
                end
            catch
                T:E:S ->
                    io:format("Error formatting ~p: ~p:~p~n~p~n",
                              [Path, T, E, S])
            end;
        Other ->
            io:format("Error: ~p~n", [Other])
    end.
