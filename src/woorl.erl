-module(woorl).

-export([main/1]).

%%

-behaviour(woody_event_handler).
-export([handle_event/3]).

%%

-define(SUCCESS     , 0).
-define(EXCEPTION   , 1).
-define(WOODY_ERROR , 2).
-define(COMPILE_ERROR   , 64).
-define(INPUT_ERROR , 128).

get_options_spec() ->
    [
        {verbose, $v, "verbose", {boolean, false},
            "Be more verbose"},
        {reqid, undefined, "reqid", {binary, get_default_reqid()},
            "Designated request identifier"},
        {schema, $s, "schema", string,
            "One or more Thrift schema definitions to use"},
        {tempdir, undefined, "tempdir", string,
            "A path to the directory which will be used to temporarily store Thrift compilation artifacts"},
        {url, undefined, undefined, string,
            "Woody service URL (e.g. 'http://svc.localhost/v1/leftpad')"},
        {service, undefined, undefined, string,
            "Woody service name (e.g. 'LeftPadder')"},
        {function, undefined, undefined, string,
            "Woody service function name (e.g. 'PadIt')"}
    ].

report_usage() ->
    getopt:usage(
        get_options_spec(), ?MODULE_STRING,
        "[<param>...]",
        [{"<param>",
            "Function parameter according to Thrift schema, represented with JSON"}]
    ),
    io:format(standard_error, "~s", [[
        "Exit status:\n",
        [format_exit_code(?SUCCESS)       , $\t, "Call succeeded"            "\n"],
        [format_exit_code(?EXCEPTION)     , $\t, "Call raised an exception"  "\n"],
        [format_exit_code(?WOODY_ERROR)   , $\t, "Call failed"               "\n"],
        [format_exit_code(?COMPILE_ERROR) , $\t, "Schema compilation failed" "\n"],
        [format_exit_code(?INPUT_ERROR)   , $\t, "Input error"               "\n"],
        "\n"
    ]]).

format_exit_code(C) ->
    genlib_string:pad_left(integer_to_binary(C), $\s, 5).

get_default_reqid() ->
    woorl_utils:unique_string(<<"woorl:">>).

%%

-spec main([string()]) -> no_return().

main(Args) ->
    ok = init_globals(),
    {ok, _} = application:ensure_all_started(?MODULE, temporary),
    {Url, Request, Schema, Opts} = parse_options(Args),
    report_call_result(issue_call(Url, Request, Opts), Schema).

parse_options(Args) ->
    case getopt:parse(get_options_spec(), Args) of
        {ok, {Opts, RestArgs}} ->
            ok = set_global(verbose, require_option(verbose, Opts)),
            prepare_options(Opts, RestArgs);
        {error, Why} ->
            abort_with_usage(Why)
    end.

prepare_options(Opts, Args) ->
    Url = require_option(url, Opts),
    SchemaPaths = lists:usort(require_options(schema, Opts)),
    ServiceName = require_option(service, Opts),
    FunctionName = require_option(function, Opts),
    Modules = prepare_schemas(SchemaPaths, Opts),
    {Service, Function, Schema} = detect_service_function(ServiceName, FunctionName, Modules),
    FunctionArgs = prepare_function_args(Args, Schema),
    {Url, {Service, Function, FunctionArgs}, Schema, Opts}.

assert_paths(Paths) ->
    [abort(?INPUT_ERROR, {invalid_file, P}) || P <- Paths, not filelib:is_regular(P)].

prepare_schemas(SchemaPaths, Opts) ->
    _ = assert_paths(SchemaPaths),
    TempPath = make_temp_dir(woorl_utils:temp_dir(genlib_opts:get(tempdir, Opts), "woorl-gen")),
    ErlPaths = generate_schemas(SchemaPaths, TempPath),
    Modules = compile_artifacts(ErlPaths),
    _ = clean_temp_dir(TempPath),
    Modules.

make_temp_dir(Path) ->
    case file:make_dir(Path) of
        ok ->
            Path;
        {error, Reason} ->
            abort(?INPUT_ERROR, {invalid_temp_dir, Path, Reason})
    end.

clean_temp_dir(Path) ->
    woorl_utils:sh("rm -rf " ++ Path).

generate_schemas(Paths, TempPath) ->
    _ = [generate_schema(P, TempPath) || P <- Paths],
    [filename:join(TempPath, P) || P <- filelib:wildcard("*.erl", TempPath)].

generate_schema(Path, TempPath) ->
    CmdArgs = ["-r", "-out", TempPath, "--gen", "erlang", Path],
    Command = string:join(["thrift" | CmdArgs], " "),
    case woorl_utils:sh(Command) of
        {ok, _} ->
            ok;
        {error, {Code, Output}} ->
            abort(?COMPILE_ERROR, {compilation_failed, Path, Code, Output})
    end.

compile_artifacts(Paths) ->
    [compile_artifact(P) || P <- Paths].

compile_artifact(Path) ->
    {ok, Module, Bin} = compile:file(Path, [binary, debug_info]),
    {module, Module} = code:load_binary(Module, Path, Bin),
    Module.

detect_service_function(ServiceName, FunctionName, Modules) ->
    Service = list_to_atom(ServiceName),
    Function = list_to_atom(FunctionName),
    case woorl_thrift:find_function(Service, Function, Modules) of
        {Module, Schema} ->
            {{Module, Service}, Function, Schema};
        notfound ->
            abort(?INPUT_ERROR, {unknown_service_function, Service, Function})
    end.

prepare_function_args(Args, Schema) ->
    Types = woorl_thrift:get_param_types(Schema),
    Json = [decode_json(A) || A <- Args],
    case {length(Json), length(Types)} of
        {L, L} ->
            [json_to_term(J, Type, N) || {N, J, Type} <- lists:zip3(lists:seq(1, L), Json, Types)];
        {L, M} ->
            abort(?INPUT_ERROR, {arguments_mismatch, L, M})
    end.

decode_json(A) ->
    try woorl_json:decode(A) catch
        error:badarg ->
            abort(?INPUT_ERROR, {invalid_json, A})
    end.

json_to_term(Json, Type, N) ->
    try woorl_json:json_to_term(Json, Type) catch
        {invalid, Where} ->
            abort(?INPUT_ERROR, {invalid_term, N, Where});
        {missing, Where} ->
            abort(?INPUT_ERROR, {missing_term, N, Where})
    end.

issue_call(Url, Request, Opts) ->
    Context = woody_client:new_context(require_option(reqid, Opts), ?MODULE),
    {Result, _} = woody_client:call_safe(Context, Request, #{url => Url}),
    Result.

report_call_result(ok, _) ->
    ok;
report_call_result({ok, Reply}, Schema) ->
    report_reply(render_reply(Reply, Schema));
report_call_result({exception, Exception}, Schema) ->
    report_exception(render_exception(Exception, Schema)),
    abort(?EXCEPTION);
report_call_result({error, Error}, _) ->
    report_error({woody_error, Error}),
    abort(?WOODY_ERROR).

render_reply(Reply, Schema) ->
    woorl_json:encode(woorl_json:term_to_json(Reply, woorl_thrift:get_reply_type(Schema))).

render_exception(Exception, Schema) ->
    ExceptionSchema = woorl_thrift:get_exception_type(Exception, Schema),
    woorl_json:encode([
        {<<"exception">>, element(1, Exception)},
        {<<"data">>, woorl_json:term_to_json(Exception, ExceptionSchema)}
    ]).

%%

-spec handle_event(
    woody_event_handler:event_type(),
    woody_t:rpc_id(),
    woody_event_handler:event_meta_type()
) -> _.

handle_event(EventType, RpcID, EventMeta) ->
    report_progress({woody, RpcID, EventType, EventMeta}).

%%

require_option(Key, Opts) ->
    case lists:keyfind(Key, 1, Opts) of
        {_, Val} ->
            Val;
        false ->
            abort_with_usage({missing_option, Key})
    end.

require_options(Key, Opts) ->
    case [Val || {K, Val} <- Opts, K =:= Key] of
        Vals when Vals =/= [] ->
            Vals;
        [] ->
            abort_with_usage({missing_option, Key})
    end.

%%

init_globals() ->
    ?MODULE = ets:new(?MODULE, [named_table, public]),
    ok.

set_global(N, V) ->
    true = ets:insert(?MODULE, {N, V}),
    ok.

get_global(N) ->
    case ets:lookup(?MODULE, N) of
        [{_, V}] ->
            V;
        [] ->
            undefined
    end.

%%

report_error(Why) ->
    {Format, Args} = format_error(Why),
    io:format(standard_error, "~s", [cf:format("~!R[ERROR]~!! " ++ Format, Args)]).

format_error({invalid_option, Opt}) ->
    {"Invalid option ~!^~s~!!~n", [Opt]};
format_error({missing_option, Key}) ->
    {"Missing required option ~!^~s~!!~n", [Key]};
format_error({invalid_file, Path}) ->
    {"Not a regular file: ~!Y~s~!!~n", [Path]};
format_error({invalid_temp_dir, Path, Why}) ->
    {"Unable to create scratch directory in ~!^~s~!!: ~!Y~p~!!~n", [Path, Why]};
format_error({compilation_failed, Path, _Code, Why}) ->
    {"Failed to compile schema ~!^~s~!!:~n~!Y~s~!!", [Path, Why]};
format_error({invalid_json, V}) ->
    {"Invalid JSON value: ~!Y~s~!!~n", [V]};
format_error({invalid_term, N, Path}) ->
    {"Parameter ~p does not conform to schema in field ~!^~s~!!~n", [N, format_path(Path)]};
format_error({missing_term, N, Path}) ->
    {"Parameter ~p does not conform to schema, missing required field ~!^~s~!!~n", [N, format_path(Path)]};
format_error({unknown_service_function, Service, Function}) ->
    {"Unable to find service ~!^~s~!! with declared function ~!^~s~!!~n",
        [Service, Function]};
format_error({arguments_mismatch, Passed, Required}) ->
    {"Function accepts ~p parameters but ~p passed~n", [Required, Passed]};
format_error({woody_error, {Class, Reason, Stacktrace}}) ->
    {"Call failed: ~!Y~s:~s~!! ~s~n",
        [Class, Reason, genlib_format:format_stacktrace(Stacktrace, [newlines])]};
format_error({woody_error, {transport_error, Why}}) ->
    {"Call failed with transport error: ~!Y~64000p~!!~n", [Why]};
format_error({woody_error, {protocol_error, Why}}) ->
    {"Call failed with protocol error: ~!Y~64000p~!!~n", [Why]};
format_error(Why) ->
    {"~!Y~p~!!~n", [Why]}.

format_path(Path) ->
    string:join(lists:map(fun format_path_part/1, Path), ".").

format_path_part(V) ->
    try genlib:to_list(V) catch
        error:_ ->
            io_lib:format("~p", [V])
    end.

report_progress(What) ->
    {Format, Args} = format_progress(What),
    get_global(verbose) andalso
        io:format(standard_error, "~s", [cf:format("~!^[DEBUG]~!! " ++ Format, Args)]).

format_progress({woody, #{trace_id := RpcID}, Event, Meta}) ->
    {"[~s] ~s: ~64000tp~n", [RpcID, Event, Meta]};
format_progress(Why) ->
    {"~p~n", [Why]}.

report_reply(R) ->
    io:format(standard_io, "~s", [R]).

report_exception(R) ->
    report_reply(R).

-spec abort_with_usage(term()) -> no_return().

abort_with_usage(Why) ->
    report_error(Why),
    report_usage(),
    abort(?INPUT_ERROR).

-spec abort(1..255, term()) -> no_return().

abort(Code, Why) ->
    report_error(Why),
    abort(Code).

-spec abort(1..255) -> no_return().

abort(Code) ->
    erlang:halt(Code).
