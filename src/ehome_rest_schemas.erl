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

-export([create_schema/2, schema_json/2]).


-record(schema, {id :: integer(), name :: binary()}).

init(Req, Opts) ->
    random:seed(now()),
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, schema_json}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, create_schema}],
        Req, State}.

resource_exists(Req, _State) ->
    case cowboy_req:binding(id, Req) of
        undefined ->
            {true, Req, index};
        ID ->
            case exists(ID) of
                true -> {true, Req, ID};
                false -> {false, Req, ID}
            end
    end.

create_schema(Req, State) ->
    {ok, [{<<"schema">>, SchemaJson}], Req2} = cowboy_req:body_qs(Req),
    Schema = json2schema(SchemaJson),
    case cowboy_req:method(Req2) of
        <<"POST">> ->
            ID = ehome_db:create_schema(Schema),
            {{true, schemaId2Url(ID)}, Req2, State};
        <<"PUT">> ->
            ID = cowboy_req:binding(id, Req2),
            {ehome_db:update_schema(ID, Schema), Req2, State};
        _ ->
            {false, Req2, State}
    end.

delete_resource(Req, State) ->
    ID = cowboy_req:binding(id, Req),
    {ehome_db:delete_schema(ID), Req, State}.


schema_json(Req, index) ->
    Json = lists:map(fun schema2json/1, ehome_db:get_schemas()),
    {jiffy:encode(Json), Req, index};
schema_json(Req, ID) ->
    {jiffy:encode(schema2json(ehome_db:get_schema(ID))), Req, ID}.

%% Privates ===================

exists(ID) ->
    case ehome_db:get_schema(ID) of
        false ->
            false;
        _ ->
            true
    end.

schemaId2Url(ID) ->
    StrID = binary:list_to_bin(integer_to_list(ID)),
    <<"/schemas/", StrID/bits>>.

schema2json(#schema{id = ID, name = Name}) ->
    Href = schemaId2Url(ID),
    #{name => Name, href => Href}.

json2schema(Json) ->
    Decoded = jiffy:decode(Json, [return_maps]),
    #{<<"name">> := Name } = Decoded,
    #schema{name = Name}.
