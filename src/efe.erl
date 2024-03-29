-module(efe).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(["pp", ConfPath | FilePaths]) ->
    each_config_and_path(ConfPath,
                         FilePaths,
                         fun (Config, FilePath) ->
                                 pprint_ex(FilePath, true, Config)
                         end,
                         ".erl");
main(["pp-lfe", ConfPath | FilePaths]) ->
    each_config_and_path(ConfPath,
                         FilePaths,
                         fun (Config, FilePath) ->
                                 pprint_lfe(FilePath, true, Config)
                         end,
                         ".lfe");
main(["pp-lfe-ast", ConfPath | FilePaths]) ->
    each_config_and_path(ConfPath,
                         FilePaths,
                         fun (Config, FilePath) ->
                                 pprint_lfe_ast(FilePath, true, Config)
                         end,
                         ".lfe");
main(["ann", ConfPath | FilePaths]) ->
    each_config_and_path(ConfPath,
                         FilePaths,
                         fun (Config, FilePath) ->
                                 {Ast, _St} = annotate(FilePath, Config),
                                 pprint({ok, Ast})
                         end,
                         ".erl");
main(["conf", ConfPath | FilePaths]) ->
    each_config_and_path(ConfPath,
                         FilePaths,
                         fun (Config, FilePath) ->
                                 io:format("~p: ~p~n~n", [FilePath, Config])
                         end,
                         ".erl");
% to make it work when transpiled to elixir, elixir escripts receive args as list
% of binaries
main(Args = [H | _]) when is_binary(H) ->
    main([binary_to_list(V) || V <- Args]);
main(_) ->
    io:format("Usage: efe pp|ann|conf path.erl+~n"),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

pprint({ok, R}) ->
    io:format("~p~n", [R]);
pprint({error, E}) ->
    io:format("Error: ~p~n", [E]).

with_ast(Path, Config, Fn) ->
    case from_erl(Path, Config) of
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

annotate(Path, Config) ->
    with_ast(Path,
             Config,
             fun (Ast) ->
                     efe_var_ann:do(Ast)
             end).

from_erl(Path,
         #{encoding := Encoding, includes := Includes, macros := Macros}) ->
    IncludePaths =
        flatten1([filelib:wildcard(IncludeBlob) || IncludeBlob <- Includes]),
    epp:parse_file(Path,
                   [{includes, IncludePaths},
                    {macros, Macros},
                    {default_encoding, Encoding}]).

pprint_ex(Path, DoPrint, Config) ->
    case from_erl(Path, Config) of
        {ok, Ast} ->
            pprint_ex_ast(Path, DoPrint, Config, Ast);
        Other ->
            io:format("Error: ~p~n", [Other])
    end.

pprint_ex_ast(Path,
              DoPrint,
              #{output_path := OutputPath, mod_prefix := ModPrefix},
              Ast) ->
    try
        {AnnAst, _St} = efe_var_ann:do(Ast),
        case DoPrint of
            true ->
                Code =
                    unicode:characters_to_binary(efe_pp:format(AnnAst,
                                                               #{mod_prefix =>
                                                                     ModPrefix}),
                                                 latin1,
                                                 utf8),
                case filelib:ensure_dir(OutputPath) of
                    ok ->
                        case file:write_file(OutputPath, Code) of
                            ok ->
                                io:format("# ~s~n", [OutputPath]),
                                ok;
                            {error, Reason} ->
                                io:format("Error writing file: ~p ~p~n",
                                          [Reason, OutputPath])
                        end;
                    {error, Reason} ->
                        io:format("Error creating file path: ~p ~p~n",
                                  [Reason, OutputPath])
                end;
            false ->
                ok
        end
    catch
        T:E:S ->
            io:format("Error formatting ~p: ~p:~p~n~p~n", [Path, T, E, S])
    end.

pprint_lfe(Path, DoPrint, Config) ->
    case lfe_comp:file(Path, [to_erlang, to_ast, binary]) of
        {ok, [{ok, _ModName, Ast}]} ->
            pprint_ex_ast(Path, DoPrint, Config, Ast);
        Other ->
            io:format("Unexpected Result Parsing LFE ~p: ~p~n", [Path, Other])
    end,
    ok.

pprint_lfe_ast(Path, _DoPrint, _Config) ->
    case lfe_comp:file(Path, [to_erlang, to_ast, binary]) of
        {ok, [{ok, _ModName, Ast}]} ->
            io:format("~p~n", [Ast]);
        Other ->
            io:format("Unexpected Result Parsing LFE ~p: ~p~n", [Path, Other])
    end,
    ok.

config_for_path(ConfPath, FilePath, Ext) ->
    {ok, [ConfigMap]} = file:consult(ConfPath),
    FileDir = filename:dirname(FilePath),
    BaseName = filename:basename(FilePath, Ext),
    DesfFileName = BaseName ++ ".ex",
    Encoding = maps:get(encoding, ConfigMap, latin1),
    ModPrefix = maps:get(mod_prefix, ConfigMap, ""),
    MacrosMap = maps:get(macros, ConfigMap, #{}),
    Macros = maps:to_list(MacrosMap),
    OutputDir = maps:get(output_dir, ConfigMap, "."),
    OutputPath =
        filename:join([OutputDir, make_relative(FileDir), DesfFileName]),
    IncludeBlobs = maps:get(includes, ConfigMap, []),
    Includes =
        [canonicalize_include_blob(FileDir, Include)
         || Include <- IncludeBlobs],
    #{encoding => Encoding,
      macros => Macros,
      includes => Includes,
      output_dir => OutputDir,
      output_path => OutputPath,
      mod_prefix => ModPrefix}.

canonicalize_include_blob(FileDir, Include = [$. | _]) ->
    filename:join([FileDir, Include]);
canonicalize_include_blob(_FileDir, Include) ->
    Include.

each_config_and_path(_ConfPath, [], _Fn, _Ext) ->
    ok;
each_config_and_path(ConfPath, [FilePath | FilePaths], Fn, Ext) ->
    Config = config_for_path(ConfPath, FilePath, Ext),
    Fn(Config, FilePath),
    each_config_and_path(ConfPath, FilePaths, Fn, Ext).

make_relative(Path = [$/ | _]) ->
    ["." | Path];
make_relative(Path) ->
    Path.

flatten1(L) ->
    flatten1(L, []).

flatten1([], Accum) ->
    lists:reverse(Accum);
flatten1([HL | T], Accum) ->
    flatten1(T, flatten1_h(HL, Accum)).

flatten1_h([], Accum) ->
    Accum;
flatten1_h([H | T], Accum) ->
    flatten1_h(T, [H | Accum]).
