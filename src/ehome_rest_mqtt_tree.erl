%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Jun 2015 8:57 AM
%%%-------------------------------------------------------------------
-module(ehome_rest_mqtt_tree).
-author("pvalsecc").

%% API
-export([init/2, allowed_methods/2, content_types_provided/2, to_json/2,
    resource_exists/2, content_types_accepted/2, from_json/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"GET">>, <<"PUT">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, to_json}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, from_json}],
        Req, State}.

resource_exists(Req, _State) ->
    Kind = cowboy_req:binding(kind, Req),
    Id = cowboy_req:binding(id, Req),
    Filter = case Kind of
        undefined -> index;
        <<"switch_binary">> -> case Id of
                 undefined -> [any, any, switch_binary, "level"];
                 _ -> ehome_utils:parse_path(Id)
             end;
        _ -> undefined
    end,
    case Filter of
        undefined -> {false, Req, index};
        index -> {true, Req, index};
        _ ->
            List = ehome_mqtt_tree:list(Filter),
            case List of
                [] -> {false, Req, index};
                _ -> {true, Req, List}
            end
    end.

to_json(Req, index) ->
    {jiffy:encode([<<"switch_binary">>]), Req, index};
to_json(Req, List) ->
    Names = ehome_names:get_map(),
    {jiffy:encode(format(List, Names)), Req, List}.

format(Entries, Names) ->
    lists:filtermap(fun(Cur) -> format_one(Cur, Names) end, Entries).

format_one({[1 | _], _Value}, _Names) ->
    false;
format_one({[DeviceId, InstanceId | _] = All, Value}, Names) ->
    Name = io_lib:format("~p/~p", [DeviceId, InstanceId]),
    BinName = list_to_binary(Name),
    Desc = maps:get([DeviceId, InstanceId], Names, BinName),
    Id = io_lib:format("~p", [All]),
    {true, #{name => BinName, desc => Desc, id => list_to_binary(Id),
             value => Value}}.

from_json(Req, [{[DeviceId, InstanceId, switch_binary, "level"], _Value}] = What) ->
    {ok, Json, Req2} = cowboy_req:body(Req),
    #{<<"desc">> := Desc} = json2sub(Json),
    ehome_names:set([DeviceId, InstanceId], Desc),
    {true, Req2, What}.

json2sub(Json) ->
    jiffy:decode(Json, [return_maps]).
