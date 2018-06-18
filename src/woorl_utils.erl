-module(woorl_utils).

-export([unique_string/1]).
-export([temp_dir/2]).
-export([parse_pretty_deadline/1]).
-export([sh/1]).

%%

-spec unique_string(binary()) -> binary().

unique_string(Prefix) ->
    <<ID:64>> = snowflake:new(?MODULE),
    IDBin = genlib_format:format_int_base(ID, 62),
    <<Prefix/binary, IDBin/binary>>.

-spec temp_dir(undefined | string(), string()) -> string().

temp_dir(TmpRoot, Prefix) ->
    Name = unique_string(genlib:to_binary(Prefix)),
    Tmp = case TmpRoot of
        undefined ->
            os:getenv("TMPDIR", "/tmp");
        _ ->
            TmpRoot
    end,
    genlib:to_list(filename:join(Tmp, Name)).

-spec sh(string() | binary()) -> {ok, string()} | {error, {1..255, string()}}.

sh(Command) ->
    PortSettings = [exit_status, {line, 16384}, use_stdio, stderr_to_stdout, hide, eof],
    Port = open_port({spawn, Command}, PortSettings),
    try
        sh_loop(Port, [])
    after
        port_close(Port)
    end.

-spec parse_pretty_deadline
    (binary()) -> {ok, woody:deadline()} | {error, bad_deadline};
    (undefined) -> {ok, undefined}.

parse_pretty_deadline(undefined) ->
    {ok, undefined};
parse_pretty_deadline(DeadlineStr) ->
    Parsers = [
        fun try_parse_woody_default/1,
        fun try_parse_relative/1
    ],
    parse_pretty_deadline(DeadlineStr, Parsers).


%%
%% Internals
%%

parse_pretty_deadline(_DeadlineStr, []) ->
    {error, bad_deadline};
parse_pretty_deadline(DeadlineStr, [P | Parsers]) ->
    case P(DeadlineStr) of
        {ok, _Deadline} = Result ->
            Result;
        {error, bad_deadline} ->
            parse_pretty_deadline(DeadlineStr, Parsers)
    end.

try_parse_woody_default(DeadlineStr) ->
    try woody_deadline:from_binary(DeadlineStr) of
        Deadline ->
            {ok, Deadline}
    catch
        error:{bad_deadline, _Reason} ->
            {error, bad_deadline}
    end.

try_parse_relative(DeadlineStr) ->
    %% deadline string like '+1ms', '+30m', '+2h' etc
    case re:run(DeadlineStr, <<"^\\+(\\d+)(\\w+)$">>) of
        {match, [_FullGroup, NumberGroup, UnitGroup]} ->
            Number = erlang:binary_to_integer(binary:part(DeadlineStr, NumberGroup)),
            Unit = binary:part(DeadlineStr, UnitGroup),
            try_parse_relative(Number, Unit);
        _Other ->
            {error, bad_deadline}
    end.

try_parse_relative(Number, <<"ms">>) ->
    {ok, woody_deadline:from_timeout(Number)};
try_parse_relative(Number, <<"s">>) ->
    {ok, woody_deadline:from_timeout(Number * 1000)};
try_parse_relative(Number, <<"m">>) ->
    {ok, woody_deadline:from_timeout(Number * 1000 * 60)};
try_parse_relative(Number, <<"h">>) ->
    {ok, woody_deadline:from_timeout(Number * 1000 * 60 * 60)};
try_parse_relative(_Number, _Unit) ->
    {error, bad_deadline}.

sh_loop(Port, Acc) ->
    receive
        {Port, {data, {eol, Line}}} ->
            sh_loop(Port, [Line ++ "\n" | Acc]);
        {Port, {data, {noeol, Line}}} ->
            sh_loop(Port, [Line | Acc]);
        {Port, eof} ->
            Data = lists:flatten(lists:reverse(Acc)),
            receive
                {Port, {exit_status, 0}} ->
                    {ok, Data};
                {Port, {exit_status, Rc}} ->
                    {error, {Rc, Data}}
            end
    end.
