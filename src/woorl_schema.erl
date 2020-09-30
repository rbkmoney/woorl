-module(woorl_schema).

-export([prepare/2]).

-type opts() :: [
    {tempdir, file:filename_all()}
].

-spec prepare([file:filename_all()], opts()) ->
    {ok, [module()]} | {error, _Reason}.
prepare(SchemaPaths, Opts) ->
    try
        _ = assert_paths(SchemaPaths),
        TempPath = make_temp_dir(woorl_utils:temp_dir(genlib_opts:get(tempdir, Opts), "woorl-gen")),
        Modules = lists:foldl(
            fun (SchemaPath, Acc) -> prepare_schema(SchemaPath, TempPath) ++ Acc end,
            [],
            SchemaPaths
        ),
        _ = clean_temp_dir(TempPath),
        {ok, Modules}
    catch
        throw:{?MODULE, Reason} ->
            {error, Reason}
    end.

assert_paths(Paths) ->
    [throw({?MODULE, {invalid_file, P}}) || P <- Paths, not filelib:is_regular(P)].

prepare_schema(SchemaPath, TempPath) ->
    prepare_schema(SchemaPath, filename:extension(SchemaPath), TempPath).

prepare_schema(SchemaPath, ".thrift", TempPath) ->
    ErlFilesWas = filelib:wildcard("*.erl", TempPath),
    ok = generate_schema(SchemaPath, TempPath),
    ErlFilesNew = filelib:wildcard("*.erl", TempPath) -- ErlFilesWas,
    ErlPaths = [filename:join(TempPath, P) || P <- ErlFilesNew],
    Modules0 = compile_artifacts(ErlPaths),
    Modules1 = filter_required_modules(Modules0, SchemaPath),
    Modules1;
prepare_schema(SchemaPath, ".beam", _) ->
    % TODO
    % This is undocumented hack for now, you can pass compiled .beam files in as schemas so that
    % woorl could skip schema compiling step altogether. All in the name of speed.
    true = code:add_pathz(filename:dirname(SchemaPath)),
    ModuleName = list_to_atom(filename:basename(SchemaPath, ".beam")),
    {module, Module} = code:load_file(ModuleName),
    [Module].

make_temp_dir(Path) ->
    case file:make_dir(Path) of
        ok ->
            Path;
        {error, Reason} ->
            throw({?MODULE, {invalid_temp_dir, Path, Reason}})
    end.

clean_temp_dir(Path) ->
    woorl_utils:sh("rm -rf " ++ Path).

generate_schema(Path, TempPath) ->
    CmdArgs = ["-r", "-out", TempPath, "--gen", "erlang:scoped_typenames", Path],
    Command = string:join(["thrift" | CmdArgs], " "),
    case woorl_utils:sh(Command) of
        {ok, _} ->
            ok;
        {error, {Code, Output}} ->
            throw({?MODULE, {compilation_failed, Path, Code, Output}})
    end.

compile_artifacts(Paths) ->
    [compile_artifact(P) || P <- Paths].

compile_artifact(Path) ->
    {ok, Module, Bin} = compile:file(Path, [binary, debug_info]),
    {module, Module} = code:load_binary(Module, Path, Bin),
    Module.

filter_required_modules(Modules, SchemaPath) ->
    SchemaName = list_to_binary(filename:basename(SchemaPath, ".thrift")),
    [M || M <- Modules, binary:match(atom_to_binary(M, utf8), SchemaName) /= nomatch].
