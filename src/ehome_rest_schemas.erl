%%%-------------------------------------------------------------------
%%% @author patrick
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Apr 2015 17:28
%%%-------------------------------------------------------------------
-module(ehome_rest_schemas).

%% API
-export([init/2, allowed_methods/2, content_types_provided/2,
    content_types_accepted/2, resource_exists/2, delete_resource/2]).

-export([from_json/2, to_json/2]).

-include("ehome_types.hrl").

init(Req, Opts) ->
    random:seed(now()),
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, to_json}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, from_json}],
        Req, State}.

resource_exists(Req, _State) ->
    case cowboy_req:binding(id, Req) of
        undefined ->
            {true, Req, index};
        Id ->
            case exists(Id) of
                true -> {true, Req, Id};
                false -> {false, Req, Id}
            end
    end.

from_json(Req, State) ->
    {ok, SchemaJson, Req2} = cowboy_req:body(Req),
    Schema = json2schema(SchemaJson),
    case cowboy_req:method(Req2) of
        <<"POST">> ->
            Id = ehome_db:create_schema(Schema),
            Req3 = cowboy_req:set_resp_header(<<"location">>,
                schemaId2Url(Id), Req2),
            io:format("~w~n", [cowboy_req:has_resp_header(<<"location">>,
                Req3)]),
            {true, Req3, State};
        <<"PUT">> ->
            Id = cowboy_req:binding(id, Req2),
            {ehome_db:update_schema(Id, Schema), Req2, State}
    end.

delete_resource(Req, State) ->
    Id = cowboy_req:binding(id, Req),
    {ehome_db:delete_schema(Id), Req, State}.


to_json(Req, index) ->
    Json = lists:map(fun schema2json/1, ehome_db:get_schemas()),
    {jiffy:encode(Json), Req, index};
to_json(Req, Id) ->
    {jiffy:encode(schema2json(ehome_db:get_schema(Id))), Req, Id}.

%% Privates ===================

exists(Id) ->
    case ehome_db:get_schema(Id) of
        false ->
            false;
        _ ->
            true
    end.

schemaId2Url(Id) ->
    StrId = binary:list_to_bin(integer_to_list(Id)),
    <<"/schemas/", StrId/bits>>.

schema2json(#schema{id = Id, name = Name}) ->
    Href = schemaId2Url(Id),
    #{name => Name, href => Href}.

json2schema(Json) ->
    Decoded = jiffy:decode(Json, [return_maps]),
    #{<<"name">> := Name } = Decoded,
    #schema{name = Name}.
