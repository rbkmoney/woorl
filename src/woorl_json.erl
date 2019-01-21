-module(woorl_json).

-export([decode/1]).
-export([encode/1]).

-export([json_to_term/2]).
-export([term_to_json/2]).

%%

-spec decode(string() | binary()) -> jsx:json_term().

decode(S) when is_binary(S) ->
    jsx:decode(S, [{labels, binary}]);
decode(S) when is_list(S) ->
    decode(unicode:characters_to_binary(S)).

-spec encode(jsx:json_term()) -> binary().

encode(J) ->
    jsx:encode(J, [space, {indent, 2}]).

%%

-define (is_integer(T) , (T == byte orelse T == i8 orelse T == i16 orelse T == i32 orelse T == i64)).
-define (is_number(T)  , (?is_integer(T) orelse T == double)).
-define (is_scalar(T)  , (?is_number(T) orelse T == string orelse element(1, T) == enum)).

-spec json_to_term(jsx:json_term(), woorl_thrift:type()) -> term().

json_to_term(Json, Type) ->
    json_to_term(Json, Type, []).

json_to_term(Json, Type, Stack) ->
    try json_to_term_(Json, Type, Stack) catch
        error:missing ->
            throw({missing, lists:reverse(Stack)});
        error:_ ->
            throw({invalid, lists:reverse(Stack)})
    end.

json_to_term_(undefined, {optional, _Type}, _Stack) ->
    undefined;
json_to_term_(undefined, {required, _Type}, _Stack) ->
    error(missing);
json_to_term_(Json, {Req, Type}, Stack) when Req == optional; Req == required ->
    json_to_term_(Json, Type, Stack);

json_to_term_(Json, {list, Type}, Stack) when is_list(Json) ->
    [json_to_term(T, Type, [N | Stack]) || {N, T} <- enumerate(0, Json)];
json_to_term_(Json, {set, Type}, Stack) when is_list(Json) ->
    ordsets:from_list(json_to_term_(Json, {list, Type}, Stack));
json_to_term_([{}], {map, KType, _VType}, _Stack) when ?is_scalar(KType) ->
    #{};
json_to_term_(Json, {map, KType, VType}, Stack) when is_list(Json), ?is_scalar(KType) ->
    lists:foldl(
        fun ({K, V}, A) ->
            A#{
                json_propkey_to_term(K, KType, [key, K | Stack]) =>
                    json_to_term(V, VType, [value, K | Stack])
            }
        end,
        #{},
        Json
    );
json_to_term_(Json, {map, KType, VType}, Stack) when is_list(Json) ->
    lists:foldl(
        fun (Pair, A) ->
            K = getv(<<"key">>, Pair),
            V = getv(<<"value">>, Pair),
            A#{
                json_to_term(K, KType, [key, K | Stack]) =>
                    json_to_term(V, VType, [value, K | Stack])
            }
        end,
        #{},
        Json
    );
json_to_term_(Json, {enum, {Mod, Name}}, Stack) when is_atom(Mod), is_atom(Name) ->
    json_to_term_(Json, Mod:enum_info(Name), Stack);
json_to_term_(Json, {enum, Fields}, _Stack) when is_list(Fields), is_binary(Json) ->
    V = binary_to_atom(Json, utf8),
    case lists:keyfind(V, 1, Fields) of
        {V, _} ->
            V;
        false ->
            error(badarg)
    end;
json_to_term_(Json, {struct, union, {Mod, Name}}, Stack) when is_atom(Mod), is_atom(Name) ->
    {struct, union, StructDef} = Mod:struct_info(Name),
    json_to_union(Json, StructDef, Stack);
json_to_term_(Json, {struct, _, {Mod, Name}}, Stack) when is_atom(Mod), is_atom(Name) ->
    {struct, _, StructDef} = Mod:struct_info(Name),
    json_to_struct(Json, StructDef, Mod:record_name(Name), Stack);
json_to_term_(Json, string, _Stack) when is_binary(Json) ->
    Json;
json_to_term_(Json = [{_, _} | _], string, _Stack) ->
    CType = getv(<<"content_type">>, Json),
    Content = getv(<<"content">>, Json),
    json_content_to_string(CType, Content);
json_to_term_(Json, bool, _Stack) when is_boolean(Json) ->
    Json;
json_to_term_(Json, double, _Stack) when is_number(Json) ->
    float(Json);
json_to_term_(Json, Type, _Stack) when
    Type == i8; Type == byte, is_integer(Json), Json >= -(1 bsl 7), Json < (1 bsl 7)
->
    Json;
json_to_term_(Json, i16, _Stack) when is_integer(Json), Json >= -(1 bsl 15), Json < (1 bsl 15) ->
    Json;
json_to_term_(Json, i32, _Stack) when is_integer(Json), Json >= -(1 bsl 31), Json < (1 bsl 31) ->
    Json;
json_to_term_(Json, i64, _Stack) when is_integer(Json), Json >= -(1 bsl 63), Json < (1 bsl 63) ->
    Json;
json_to_term_(_Json, _Type, _Stack) ->
    error(badarg).

json_propkey_to_term(P, Type = string, Stack) ->
    json_to_term_(P, Type, Stack);
json_propkey_to_term(P, Type = {enum, _}, Stack) ->
    json_to_term_(P, Type, Stack);
json_propkey_to_term(P, Type, Stack) when ?is_integer(Type) ->
    json_to_term_(binary_to_integer(P), Type, Stack);
json_propkey_to_term(P, Type = double, Stack) ->
    json_to_term_(try binary_to_float(P) catch error:badarg -> binary_to_integer(P) end, Type, Stack).

json_to_struct(Json, StructDef, RecordName, Stack) when is_list(Json) ->
    list_to_tuple([RecordName | lists:map(
        fun ({_N, Req, Type, Fn, Def}) ->
            FJson = getv(atom_to_binary(Fn, utf8), Json, Def),
            json_to_term(FJson, {Req, Type}, [Fn | Stack])
        end,
        StructDef
    )]).

json_to_union([{FnBin, Json}], StructDef, Stack) ->
    {_N, _Req, Type, Fn, _Def} = lists:keyfind(binary_to_atom(FnBin, utf8), 4, StructDef),
    {Fn, json_to_term(Json, Type, [Fn | Stack])}.

json_content_to_string(<<"base64">>, Content) ->
    base64:decode(Content).

%%

-spec term_to_json(term(), woorl_thrift:type()) -> jsx:json_term().

term_to_json(Term, Type) ->
    term_to_json(Term, Type, []).

term_to_json(Term, {list, Type}, Stack) when is_list(Term) ->
    [term_to_json(T, Type, [N | Stack]) || {N, T} <- enumerate(0, Term)];
term_to_json(Term, {set, Type}, Stack) ->
    term_to_json(ordsets:to_list(Term), {list, Type}, Stack);
term_to_json(Term, {map, KType, VType}, Stack) when is_map(Term), ?is_scalar(KType) ->
    maps:fold(
        fun (K, V, A) ->
            [{genlib:to_binary(K), term_to_json(V, VType, [value, V | Stack])} | A]
        end,
        [],
        Term
    );
term_to_json(Term, {map, KType, VType}, Stack) when is_map(Term) ->
    maps:fold(
        fun (K, V, A) ->
            [[
                {<<"key">>, term_to_json(K, KType, [key, K | Stack])},
                {<<"value">>, term_to_json(V, VType, [value, V | Stack])}
            ] | A]
        end,
        [],
        Term
    );
term_to_json(Term, {struct, union, {Mod, Name}}, Stack) when is_atom(Mod), is_atom(Name) ->
    {struct, _, StructDef} = Mod:struct_info(Name),
    union_to_json(Term, StructDef, Stack);
term_to_json(Term, {struct, _, {Mod, Name}}, Stack) when is_atom(Mod), is_atom(Name), is_tuple(Term) ->
    {struct, _, StructDef} = Mod:struct_info(Name),
    struct_to_json(Term, StructDef, Stack);
term_to_json(Term, {enum, _}, _Stack) when is_atom(Term) ->
    atom_to_binary(Term, utf8);
term_to_json(Term, Type, _Stack) when is_integer(Term), ?is_integer(Type) ->
    Term;
term_to_json(Term, double, _Stack) when is_number(Term) ->
    float(Term);
term_to_json(Term, string, _Stack) when is_binary(Term) ->
    case unicode:characters_to_list(Term) of
        L when is_list(L) ->
            Term;
        _ ->
            term_to_json_content(Term)
    end;
term_to_json(Term, bool, _Stack) when is_boolean(Term) ->
    Term;
term_to_json(Term, Type, _Stack) ->
    error({badarg, Term, Type}).

union_to_json({Fn, Term}, StructDef, Stack) ->
    {_N, _Req, Type, Fn, _Def} = lists:keyfind(Fn, 4, StructDef),
    [{atom_to_binary(Fn, utf8), term_to_json(Term, Type, [Fn | Stack])}].

struct_to_json(Struct, StructDef, Stack) ->
    [_ | Fields] = tuple_to_list(Struct),
    lists:foldr(
        fun
            ({undefined, _}, A) ->
                A;
            ({Term, {_N, _Req, Type, Fn, _Def}}, A) ->
                [{atom_to_binary(Fn, utf8), term_to_json(Term, Type, [Fn | Stack])} | A]
        end,
        [],
        lists:zip(Fields, StructDef)
    ).

term_to_json_content(Term) ->
    term_to_json_content(<<"base64">>, base64:encode(Term)).

term_to_json_content(CType, Term) ->
    [
        {<<"content_type">>, CType},
        {<<"content">>, Term}
    ].

%%

enumerate(_, []) ->
    [];
enumerate(N, [H | T]) ->
    [{N, H} | enumerate(N + 1, T)].


getv(Key, Opts) ->
    getv(Key, Opts, undefined).

getv(Key, Opts, Default) ->
    case lists:keyfind(Key, 1, Opts) of
        {_, Value} -> Value;
        false -> Default
    end.
