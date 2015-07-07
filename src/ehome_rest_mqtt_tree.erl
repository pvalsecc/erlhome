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
-export([init/2, allowed_methods/2, content_types_provided/2, to_json/2, resource_exists/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, to_json}
    ], Req, State}.

resource_exists(Req, _State) ->
    Kind = cowboy_req:binding(kind, Req),
    case Kind of
        undefined -> {true, Req, index};
        <<"switch_binary">> -> {true, Req, [any, any, switch_binary, "level"]};
        _ -> {false, Req, index}
    end.

to_json(Req, index) ->
    {jiffy:encode([<<"switch_binary">>]), Req, index};
to_json(Req, Filter) ->
    List = ehome_mqtt_tree:list(Filter),
    Names = ehome_names:get_map(),
    {jiffy:encode(format(List, Names)), Req, Filter}.

format([], _Names) ->
    [];
format([[DeviceId, InstanceId | _] = All | Rest], Names) ->
    Name = io_lib:format("~p/~p", [DeviceId, InstanceId]),
    BinName = list_to_binary(Name),
    Desc = maps:get(BinName, Names, BinName),
    Id = io_lib:format("~p", [All]),
    [#{name => BinName, desc => Desc, id => list_to_binary(Id)} |
        format(Rest, Names)].
