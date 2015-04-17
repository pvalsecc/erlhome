%%%-------------------------------------------------------------------
%%% @author patrick
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Apr 2015 17:28
%%%-------------------------------------------------------------------
-module(ehome_rest_connections).

-export([sub2json/2]).

-include("ehome_rest_sub_common.hrl").


%% Privates ===================

create(SchemaId, Sub) ->
    ehome_db:create_connection(SchemaId, Sub).

update(SchemaId, SubId, Sub) ->
    ehome_db:update_connection(SchemaId, SubId, Sub).

delete(SchemaId, SubId) ->
    ehome_db:delete_connection(SchemaId, SubId).

get_subs(SchemaId) ->
    #schema{connections = Subs} = ehome_db:get_schema(SchemaId),
    Subs.

get_sub(SchemaId, SubId) ->
    ehome_db:get_connection(SchemaId, SubId).

subId2Url(SchemaId, SubId) ->
    StrSchemaId = ehome_utils:id2str(SchemaId),
    StrSubId = ehome_utils:id2str(SubId),
    <<"/schemas/", StrSchemaId/bytes, "/connections/", StrSubId/bytes>>.

sub2json(SchemaId, #connection{id = Id, source_id = SourceId, source_output = SourceOutput,
                               target_id = TargetId, target_input = TargetInput}) ->
    Href = subId2Url(SchemaId, Id),
    #{id => Id, source_id => SourceId, source_output => SourceOutput,
      target_id => TargetId, target_input => TargetInput, href => Href}.

json2sub(Json) ->
    Decoded = jiffy:decode(Json, [return_maps]),
    #{<<"source_id">> := SourceId, <<"source_output">> := SourceOutput,
      <<"target_id">> := TargetId, <<"target_input">> := TargetInput } = Decoded,
    #connection{source_id = SourceId, source_output = SourceOutput,
                target_id = TargetId, target_input = TargetInput}.
