%%%-------------------------------------------------------------------
%%% @author patrick
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Apr 2015 17:28
%%%-------------------------------------------------------------------
-module(ehome_rest_elements).

-export([sub2json/2]).

-include("ehome_rest_sub_common.hrl").


%% Privates ===================

create(SchemaId, Sub) ->
    ehome_db:create_element(SchemaId, Sub).

update(SchemaId, SubId, Sub) ->
    ehome_db:update_element(SchemaId, SubId, Sub).

delete(SchemaId, SubId) ->
    ehome_db:delete_element(SchemaId, SubId).

get_subs(SchemaId) ->
    #schema{elements = Subs} = ehome_db:get_schema(SchemaId),
    Subs.

get_sub(SchemaId, SubId) ->
    ehome_db:get_element(SchemaId, SubId).

subId2Url(SchemaId, SubId) ->
    StrSchemaId = ehome_utils:id2str(SchemaId),
    StrSubId = ehome_utils:id2str(SubId),
    <<"/schemas/", StrSchemaId/bytes, "/elements/", StrSubId/bytes>>.

sub2json(SchemaId, #element{id = Id, type = Type, x = X, y = Y}) ->
    Href = subId2Url(SchemaId, Id),
    #{type => Type, x => X, y => Y, href => Href}.

json2sub(Json) ->
    Decoded = jiffy:decode(Json, [return_maps]),
    #{<<"type">> := Type, <<"x">> := X, <<"y">> := Y } = Decoded,
    #element{type = Type, x = X, y = Y}.
