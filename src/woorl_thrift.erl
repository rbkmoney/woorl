-module(woorl_thrift).

%%

-type type() ::
    base_type()
    | collection_type()
    | enum_type()
    | struct_type().

-type base_type() ::
    bool
    | double
    | i8
    | i16
    | i32
    | i64
    | string.

-type collection_type() ::
    {list, type()}
    | {set, type()}
    | {map, type(), type()}.

-type enum_type() ::
    {enum, type_ref()}.

-type struct_type() ::
    {struct, struct_flavor(), type_ref()}.

-type struct_flavor() :: struct | union | exception.

-type type_ref() :: {module(), Name :: atom()}.

-export_type([type/0]).

%%

-export([find_function/3]).
-export([get_function_schema/3]).
-export([get_param_types/1]).
-export([get_reply_type/1]).
-export([get_exception_type/2]).

%%

-opaque function_schema() :: tuple().

-export_type([function_schema/0]).

-spec find_function(woody:service_name(), woody:func(), [module()]) -> {module(), function_schema()} | notfound.
find_function(Service, Function, [Module | Rest]) ->
    case check_function(Service, Function, Module) of
        {Module, Schema} ->
            {Module, Schema};
        undefined ->
            find_function(Service, Function, Rest)
    end;
find_function(_Service, _Function, []) ->
    notfound.

check_function(Service, Function, Module) ->
    try
        {Module, get_function_schema(Module, Service, Function)}
    catch
        error:badarg ->
            undefined
    end.

-spec get_function_schema(module(), woody:service_name(), woody:func()) -> function_schema().
get_function_schema(Module, Service, Function) ->
    list_to_tuple([Module:function_info(Service, Function, T) || T <- [params_type, reply_type, exceptions]]).

-spec get_param_types(function_schema()) -> [type()].
get_param_types({{struct, _, SchemaDef}, _, _}) ->
    [Type || {_N, _Req, Type, _Name, _Def} <- SchemaDef].

-spec get_reply_type(function_schema()) -> type().
get_reply_type({_, ReplyType, _}) ->
    ReplyType.

-spec get_exception_type(tuple(), function_schema()) -> {Name :: atom(), type()}.
get_exception_type(Exception, {_, _, {struct, _, StructDef}}) ->
    ExceptionName = element(1, Exception),
    hd([
        {En, Type}
     || {_N, _Req, {struct, exception, {Mod, En}} = Type, _Name, _} <- StructDef,
        Mod:record_name(En) =:= ExceptionName
    ]).
