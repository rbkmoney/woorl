-module(woorl_utils).

-export([unique_string/1]).
-export([temp_dir/2]).

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
