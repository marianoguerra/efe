-module(efe).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(["pp", Path]) ->
    pprint_ex(Path);
main(_) ->
    io:format("Usage: efe pp <path.ex>"),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

from_erl(Path) -> epp:parse_file(Path, [], []).

pprint_ex(Path) ->
    case from_erl(Path) of
        {ok, Ast} ->
            try
                io:format("~s~n", [efe_pp:format(Ast)])
            catch T:E:S ->
                io:format("Error formatting ~p: ~p:~p~n~p~n", [Path, T, E, S])
            end;
        Other -> io:format("Error: ~p~n", [Other])
    end.
