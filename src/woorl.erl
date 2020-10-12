-module(woorl).
-mode(compile).

-export([main/1]).

%%

-behaviour(woody_event_handler).
-export([handle_event/4]).

%%

-define(SUCCESS       , 0).
-define(EXCEPTION     , 1).
-define(WOODY_ERROR   , 2).
-define(COMPILE_ERROR , 64).
-define(INPUT_ERROR   , 128).

get_options_spec(woorl) ->
    [
        {help, $h, "help", undefined,
            "Print this message and exit"},
        {verbose, $v, "verbose", {boolean, false},
            "Be more verbose"},
        {reqid, undefined, "reqid", {binary, get_default_reqid()},
            "Designated request identifier"},
        {schema, $s, "schema", string,
            "One or more Thrift schema definitions to use"},
        {user_id, $u, "user-id", string,
            "ID of the user on whose behalf the call is being made"},
        {user_name, undefined, "user-name", string,
            "Name of the user identified with user-id, ignored when user-id is not set"},
        {user_email, undefined, "user-email", string,
            "Email address of the user identified with user-id, ignored when user-id is not set"},
        {user_realm, $r, "user-realm", string,
            "The realm of the user on whose behalf the call is being made, "
            "must be present if the user ID is specified"},
        {tempdir, undefined, "tempdir", string,
            "A path to the directory which will be used to temporarily store Thrift compilation artifacts"},
        {deadline, undefined, "deadline", binary,
            "The request deadline, either absolute (e.g. '1990-12-31T23:59:60.123123Z')"
            "or relative (e.g. '15h', '3000ms', '3.5d' etc). Known units are 'ms', 's', 'm', 'h', 'd'."},
        {url, undefined, undefined, string,
            "Woody service URL (e.g. 'http://svc.localhost/v1/leftpad')"},
        {service, undefined, undefined, string,
            "Woody service name (e.g. 'LeftPadder')"},
        {function, undefined, undefined, string,
            "Woody service function name (e.g. 'PadIt')"}
    ];

get_options_spec(woorl_json) ->
    [
        {help, $h, "help", undefined,
            "Print this message and exit"},
        {schema, $s, "schema", string,
            "Thrift schema definition to use"},
        {type, $t, "type", atom,
            "Thrift type to use, e.g. 'ComplexStruct'"},
        {tempdir, undefined, "tempdir", string,
            "A path to the directory which will be used to temporarily store Thrift compilation artifacts"},
        {decode, $d, "decode", boolean,
            "Decode a Thrift binary into JSON representation of a Thrift term (this is default mode)"},
        {encode, $e, "encode", boolean,
            "Encode a JSON representation of Thrift term into a Thrift binary"}
    ].

report_usage(woorl) ->
    print_version(),
    getopt:usage(
        get_options_spec(woorl), ?MODULE_STRING,
        "[<param>...]",
        [{
            "<param>",
            "Function parameter according to Thrift schema, represented with JSON. "
            "If it starts with `@' symbol then the rest will be interpreted as a name of a file containing JSON value."
        }]
    ),
    report_exit_codes([
        ?SUCCESS,
        ?EXCEPTION,
        ?WOODY_ERROR,
        ?COMPILE_ERROR,
        ?INPUT_ERROR
    ]);

report_usage(woorl_json) ->
    print_version(),
    getopt:usage(
        get_options_spec(woorl_json), "woorl-json",
        []
    ),
    report_exit_codes([
        ?SUCCESS,
        ?COMPILE_ERROR,
        ?INPUT_ERROR
    ]).

print_version() ->
    {ok, Vsn} = application:get_key(?MODULE, vsn),
    io:format(standard_error, "~s ~s~n~n", [?MODULE_STRING, Vsn]).

report_exit_codes(Codes) ->
    io:format(standard_error, "~s", [[
        "Exit status:\n" |
        [[pad_exit_code(Code), $\t, format_exit_code(Code), $\n] || Code <- Codes]
    ]]).

format_exit_code(?SUCCESS)       -> "Call succeeded";
format_exit_code(?EXCEPTION)     -> "Call raised an exception";
format_exit_code(?WOODY_ERROR)   -> "Call failed";
format_exit_code(?COMPILE_ERROR) -> "Schema compilation failed";
format_exit_code(?INPUT_ERROR)   -> "Input error".

pad_exit_code(C) ->
    genlib_string:pad_left(integer_to_binary(C), $\s, 5).

get_default_reqid() ->
    woorl_utils:unique_string(<<"woorl:">>).

%%

-spec main([string()]) -> no_return().

main(Args) ->
    ok = configure_logger(),
    ok = init_globals(),
    {ok, _} = application:ensure_all_started(?MODULE),
    ScriptName = filename:basename(escript:script_name()),
    case ScriptName of
        % TODO
        % Dispatch this in different modules, it's coupled too tightly right now.
        "woorl-json" ->
            Script = woorl_json,
            ok = set_global(script, Script),
            {Op, Type, Input} = parse_options(Script, Args),
            io:format(standard_io, "~s", [convert_input(Op, Type, Input)]);
        _Otherwise ->
            Script = woorl,
            ok = set_global(script, Script),
            {Url, Request, Schema, Opts} = parse_options(Script, Args),
            report_call_result(issue_call(Url, Request, Opts), Schema)
    end.

parse_options(Script, Args) ->
    case getopt:parse(get_options_spec(Script), Args) of
        {ok, {Opts, RestArgs}} ->
            _ = has_option(help, Opts) andalso exit_with_usage(),
            ok = set_global(verbose, get_option(verbose, Opts) == true),
            prepare_options(Script, Opts, RestArgs);
        {error, Why} ->
            abort_with_usage(Why)
    end.

prepare_options(woorl, Opts, Args) ->
    Url = require_option(url, Opts),
    SchemaPaths = require_options(schema, Opts), % we deliberately do not care about duplicates here
    ServiceName = require_option(service, Opts),
    FunctionName = require_option(function, Opts),
    Modules = prepare_schemas(SchemaPaths, Opts),
    {Service, Function, Schema} = detect_service_function(ServiceName, FunctionName, Modules),
    FunctionArgs = prepare_function_args(Args, Schema),
    {Url, {Service, Function, FunctionArgs}, Schema, Opts};

prepare_options(woorl_json, Opts, []) ->
    TypeName = require_option(type, Opts),
    SchemaPath = require_option(schema, Opts),
    Op = detect_operation(get_option(encode, Opts), get_option(decode, Opts)),
    Modules = prepare_schemas([SchemaPath], Opts),
    Type = detect_type(TypeName, Modules),
    {Op, Type, woorl_utils:read_input()};
prepare_options(woorl_json, _Opts, ExtraArgs) ->
    abort_with_usage({invalid_extra_args, ExtraArgs}).

detect_operation(true, undefined) ->
    encode;
detect_operation(undefined, _) ->
    decode;
detect_operation(true, true) ->
    abort(?INPUT_ERROR, {invalid_option, "decode + encode"}).

detect_type(TypeName, [Module | Rest]) ->
    try Module:struct_info(TypeName) of
        {struct, Flavor, _StructDef} ->
            {struct, Flavor, {Module, TypeName}}
    catch
        error:badarg ->
            detect_type(TypeName, Rest)
    end;
detect_type(TypeName, []) ->
    abort(?INPUT_ERROR, {invalid_type, TypeName}).

prepare_schemas(SchemaPaths, Opts) ->
    case woorl_schema:prepare(SchemaPaths, Opts) of
        {ok, Modules} ->
            Modules;
        {error, {invalid_file, _} = Reason} ->
            abort(?INPUT_ERROR, Reason);
        {error, {invalid_temp_dir, _, _} = Reason} ->
            abort(?INPUT_ERROR, Reason);
        {error, {compilation_failed, _, _, _} = Reason} ->
            abort(?COMPILE_ERROR, Reason)
    end.

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
    Json = [decode_json(read_arg_contents(A)) || A <- Args],
    case {length(Json), length(Types)} of
        {L, L} ->
            list_to_tuple([json_to_term(J, Type, N) || {N, J, Type} <- lists:zip3(lists:seq(1, L), Json, Types)]);
        {L, M} ->
            abort(?INPUT_ERROR, {arguments_mismatch, L, M})
    end.

read_arg_contents([$@ | Name]) ->
    {ok, Bin} = file:read_file(Name),
    Bin;
read_arg_contents(A) ->
    A.

decode_json(A) ->
    try woorl_json:decode(A) catch
        error:badarg ->
            abort(?INPUT_ERROR, {invalid_json, A})
    end.

decode_thrift(V, Type) ->
    Codec = thrift_strict_binary_codec:new(V),
    case thrift_strict_binary_codec:read(Codec, Type) of
        {ok, Term, CodecLeft} ->
            _ = case thrift_strict_binary_codec:close(CodecLeft) of
                <<>> ->
                    ok;
                Leftovers ->
                    report_error({excess_thrift_bytes, Leftovers})
            end,
            Term;
        {error, Reason} ->
            abort(?INPUT_ERROR, {invalid_thrift, Reason})
    end.

json_to_term(Json, Type, N) ->
    try woorl_json:json_to_term(Json, Type) catch
        {invalid, Where} ->
            abort(?INPUT_ERROR, {invalid_term, N, Where});
        {missing, Where} ->
            abort(?INPUT_ERROR, {missing_term, N, Where})
    end.

issue_call(Url, Request, Opts) ->
    ReqID = require_option(reqid, Opts),
    RpcID = woody_context:new_rpc_id(<<"undefined">>, ReqID, woody_context:new_req_id()),
    Context = apply_options_to_context(Opts, woody_context:new(RpcID)),
    CallOpts = #{url => Url, event_handler => ?MODULE},
    try woody_client:call(Request, CallOpts, Context) catch
        error:{woody_error, Reason} ->
            {error, Reason}
    end.

apply_options_to_context(Opts, Context) ->
    attach_deadline(Opts, attach_user_identity(Opts, Context)).

attach_user_identity(Opts, Context) ->
    case get_option(user_id, Opts) of
        ID when ID /= undefined ->
            Realm = require_option(user_realm, Opts),
            Identity1 = #{
                id => list_to_binary(ID),
                realm => list_to_binary(Realm)
            },
            Identity2 = maps:fold(
                fun
                    (_, undefined, Identity) -> Identity;
                    (K, V, Identity)         -> Identity#{K => list_to_binary(V)}
                end,
                Identity1,
                #{
                    username => get_option(user_name, Opts),
                    email => get_option(user_email, Opts)
                }
            ),
            woody_user_identity:put(Identity2, Context);
        undefined ->
            Context
    end.

attach_deadline(Opts, Context) ->
    case get_deadline(Opts) of
        Deadline when Deadline /= undefined ->
            woody_context:set_deadline(Deadline, Context);
        undefined ->
            Context
    end.

get_deadline(Opts) ->
    case woorl_deadline:parse_pretty(get_option(deadline, Opts)) of
        {ok, Deadline} ->
            Deadline;
        {error, bad_deadline} ->
            abort_with_usage({invalid_format, deadline})
    end.

report_call_result({ok, ok}, _) ->
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
    {Name, ExceptionSchema} = woorl_thrift:get_exception_type(Exception, Schema),
    woorl_json:encode([
        {<<"exception">>, Name},
        {<<"data">>, woorl_json:term_to_json(Exception, ExceptionSchema)}
    ]).

convert_input(decode, Type, Input) ->
    Term = decode_thrift(Input, Type),
    woorl_json:encode(woorl_json:term_to_json(Term, Type));
convert_input(encode, Type, Input) ->
    Term = json_to_term(decode_json(Input), Type),
    Codec = thrift_strict_binary_codec:new(<<>>),
    {ok, Codec1} = thrift_strict_binary_codec:write(Codec, Type, Term),
    thrift_strict_binary_codec:close(Codec1).

json_to_term(Json, Type) ->
    try woorl_json:json_to_term(Json, Type) catch
        {invalid, Where} ->
            abort(?INPUT_ERROR, {invalid_term, Where});
        {missing, Where} ->
            abort(?INPUT_ERROR, {missing_term, Where})
    end.

%%

-spec handle_event(Event, RpcId, Meta, Opts) -> ok when
    Event :: woody_event_handler:event(),
    RpcId :: woody:rpc_id() | undefined,
    Meta  :: woody_event_handler:event_meta(),
    Opts  :: woody:options().

handle_event(Event, RpcID, Meta, _Opts) ->
    report_progress({woody, RpcID, Event, Meta}).

%%

has_option(Key, Opts) ->
    lists:member(Key, Opts) orelse lists:keyfind(Key, 1, Opts) /= false.

get_option(Key, Opts) ->
    case lists:keyfind(Key, 1, Opts) of
        {_, Val} ->
            Val;
        false ->
            undefined
    end.

require_option(Key, Opts) ->
    case get_option(Key, Opts) of
        Val when Val /= undefined ->
            Val;
        undefined ->
            abort_with_usage({missing_option, Key})
    end.

require_options(Key, Opts) ->
    case [Val || {K, Val} <- Opts, K =:= Key] of
        Vals when Vals =/= [] ->
            Vals;
        [] ->
            abort_with_usage({missing_option, Key})
    end.

configure_logger() ->
    Handlers = [
        #{
            id => default,
            module => logger_std_h,
            level => debug,
            config => #{
                type => standard_error
            },
            formatter => {logger_formatter, #{}}
        }
    ],
    ok = lists:foreach(
        fun(#{id := HandlerID}) ->
            ok = logger:remove_handler(HandlerID)
        end,
        logger:get_handler_config()
    ),
    ok = lists:foreach(
        fun(#{id := HandlerID, module := Module} = Config) ->
            ok = logger:add_handler(HandlerID, Module, Config)
        end,
        Handlers
    ),
    ok.

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
format_error({invalid_extra_args, Args}) ->
    {"Extra command line arguments are unexpected: ~!^~p~!!~n", [Args]};
format_error({missing_option, Key}) ->
    {"Missing required option ~!^~s~!!~n", [Key]};
format_error({invalid_format, Opt}) ->
    {"Invalid ~!^~p~!! option format~n", [Opt]};
format_error({invalid_file, Path}) ->
    {"Not a regular file: ~!Y~s~!!~n", [Path]};
format_error({invalid_temp_dir, Path, Why}) ->
    {"Unable to create scratch directory in ~!^~s~!!: ~!Y~p~!!~n", [Path, Why]};
format_error({compilation_failed, Path, _Code, Why}) ->
    {"Failed to compile schema ~!^~s~!!:~n~!Y~s~!!", [Path, Why]};
format_error({invalid_json, V}) ->
    {"Invalid JSON value: ~!Y~s~!!~n", [V]};
format_error({invalid_thrift, Reason}) ->
    {"Thrift binary does not parse: ~!^~0p~!!~n", [Reason]};
format_error({excess_thrift_bytes, Bytes}) ->
    {"Thrift binary has extra bytes at the end: ~!^~0p~!!~n", [Bytes]};
format_error({invalid_term, N, Path}) ->
    {"Parameter ~p does not conform to schema in field ~!^~s~!!~n", [N, format_path(Path)]};
format_error({invalid_term, Path}) ->
    {"Input does not conform to schema in field ~!^~s~!!~n", [format_path(Path)]};
format_error({missing_term, N, Path}) ->
    {"Parameter ~p does not conform to schema, missing required field ~!^~s~!!~n", [N, format_path(Path)]};
format_error({missing_term, Path}) ->
    {"Input does not conform to schema, missing required field ~!^~s~!!~n", [format_path(Path)]};
format_error({unknown_service_function, Service, Function}) ->
    {"Unable to find service ~!^~s~!! with declared function ~!^~s~!!~n",
        [Service, Function]};
format_error({arguments_mismatch, Passed, Required}) ->
    {"Function accepts ~p parameters but ~p passed~n", [Required, Passed]};
format_error({woody_error, {_Source, Class, Details}}) ->
    {format_error_class(Class) ++ ": ~!^~s~!!~n", [Details]};
format_error(Why) ->
    {"~!Y~p~!!~n", [Why]}.

format_error_class(result_unexpected) ->
    "Received ~!Yunexpected~!! result";
format_error_class(resource_unavailable) ->
    "Resource ~!Yunavailable~!!";
format_error_class(result_unknown) ->
    "Result ~!Yunknown~!!".

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
    report_usage(get_global(script)),
    abort(?INPUT_ERROR).

-spec exit_with_usage() -> no_return().

exit_with_usage() ->
    report_usage(get_global(script)),
    abort(?SUCCESS).

-spec abort(1..255, term()) -> no_return().

abort(Code, Why) ->
    report_error(Why),
    abort(Code).

-spec abort(1..255) -> no_return().

abort(Code) ->
    erlang:halt(Code).
