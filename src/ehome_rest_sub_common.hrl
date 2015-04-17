%%%-------------------------------------------------------------------
%%% @author patrick
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%% Common function implementation for Schema sub elements
%%% @end
%%% Created : 16. Apr 2015 09:56
%%%-------------------------------------------------------------------
-author("patrick").

%% API
-export([resource_exists/2, delete_resource/2]).

-export([from_json/2, to_json/2]).

-include_lib("mixer/include/mixer.hrl").
-mixin([ehome_rest_base]).

-include("ehome_types.hrl").

resource_exists(Req, _State) ->
    SchemaId = cowboy_req:binding(schema_id, Req),
    case cowboy_req:binding(sub_id, Req) of
        undefined ->
            {ehome_rest_schemas:exists(SchemaId), Req, {SchemaId, index}};
        SubId ->
            {exists(SchemaId, SubId), Req, {SchemaId, SubId}}
    end.

exists(SchemaId, SubId) ->
    case get_sub(SchemaId, SubId) of
        false -> false;
        _     -> true
    end.

from_json(Req, State) ->
    {ok, SubJson, Req2} = cowboy_req:body(Req),
    SchemaId = cowboy_req:binding(schema_id, Req),
    Sub = json2sub(SubJson),
    case cowboy_req:method(Req2) of
        <<"POST">> ->
            SubId = create(SchemaId, Sub),
            Req3 = cowboy_req:set_resp_header(<<"location">>,
                subId2Url(SchemaId, SubId), Req2),
            Req4 = cowboy_req:set_resp_body(
                jiffy:encode(sub2json(SchemaId, get_sub(SchemaId, SubId))), Req3),
            {true, Req4, State};
        <<"PUT">> ->
            SubId = cowboy_req:binding(sub_id, Req2),
            {update(SchemaId, SubId, Sub), Req2, State}
    end.

delete_resource(Req, State) ->
    SchemaId = cowboy_req:binding(schema_id, Req),
    SubId = cowboy_req:binding(sub_id, Req),
    {delete(SchemaId, SubId), Req, State}.

to_json(Req, {SchemaId, index}) ->
    Subs = get_subs(SchemaId),
    Json = lists:map(fun(Sub) -> sub2json(SchemaId, Sub) end, Subs),
    {jiffy:encode(Json), Req, index};
to_json(Req, {SchemaId, SubId}) ->
    Sub = get_sub(SchemaId, SubId),
    {jiffy:encode(sub2json(SchemaId, Sub)), Req, SubId}.
