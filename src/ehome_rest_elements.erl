%%%-------------------------------------------------------------------
%%% @author patrick
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Apr 2015 17:28
%%%-------------------------------------------------------------------
-module(ehome_rest_elements).

%% API
-export([resource_exists/2, delete_resource/2]).

-export([from_json/2, to_json/2, sub2json/2]).

-include_lib("mixer/include/mixer.hrl").
-mixin([ehome_rest_base]).

-include("ehome_types.hrl").

resource_exists(Req, _State) ->
    SchemaId = cowboy_req:binding(schema_id, Req),
    case cowboy_req:binding(sub_id, Req) of
        undefined ->
            {exists(SchemaId), Req, {SchemaId, index}};
        SubId ->
            {exists(SchemaId, SubId), Req, {SchemaId, SubId}}
    end.

from_json(Req, State) ->
    {ok, SubJson, Req2} = cowboy_req:body(Req),
    SchemaId = cowboy_req:binding(schema_id, Req),
    Sub = json2sub(SubJson),
    case cowboy_req:method(Req2) of
        <<"POST">> ->
            SubId = ehome_db:create_element(SchemaId, Sub),
            Req3 = cowboy_req:set_resp_header(<<"location">>,
                subId2Url(SchemaId, SubId), Req2),
            {true, Req3, State};
        <<"PUT">> ->
            SubId = cowboy_req:binding(sub_id, Req2),
            {ehome_db:update_element(SchemaId, SubId, Sub), Req2, State}
    end.

delete_resource(Req, State) ->
    SchemaId = cowboy_req:binding(schema_id, Req),
    SubId = cowboy_req:binding(sub_id, Req),
    {ehome_db:delete_element(SchemaId, SubId), Req, State}.

to_json(Req, {SchemaId, index}) ->
    #schema{elements = Subs} = ehome_db:get_schema(SchemaId),
    Json = lists:map(fun(Sub) -> sub2json(SchemaId, Sub) end, Subs),
    {jiffy:encode(Json), Req, index};
to_json(Req, {SchemaId, SubId}) ->
    Sub = ehome_db:get_element(SchemaId, SubId),
    {jiffy:encode(sub2json(SchemaId, Sub)), Req, SubId}.

%% Privates ===================

exists(SchemaId) ->
    case ehome_db:get_schema(SchemaId) of
        false -> false;
        _     -> true
    end.

exists(SchemaId, SubId) ->
    case ehome_db:get_element(SchemaId, SubId) of
        false -> false;
        _     -> true
    end.

id2str(Id) ->
    binary:list_to_bin(integer_to_list(Id)).

subId2Url(SchemaId, SubId) ->
    StrSchemaId = id2str(SchemaId),
    StrSubId = id2str(SubId),
    <<"/schemas/", StrSchemaId/bytes, "/elements/", StrSubId/bytes>>.

sub2json(SchemaId, #element{id = Id, type = Type, x = X, y = Y}) ->
    Href = subId2Url(SchemaId, Id),
    #{type => Type, x => X, y => Y, href => Href}.

json2sub(Json) ->
    Decoded = jiffy:decode(Json, [return_maps]),
    #{<<"type">> := Type, <<"x">> := X, <<"y">> := Y } = Decoded,
    #element{type = Type, x = X, y = Y}.
