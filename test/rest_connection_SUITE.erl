%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2015 11:44 AM
%%%-------------------------------------------------------------------
-module(rest_connection_SUITE).

-include_lib("common_test/include/ct.hrl").

%% API
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([create/1, delete/1, update/1]).

-export([create_connection/5]).

all() ->
    [create, delete, update].

init_per_testcase(TestCase, Config) ->
    rest_element_SUITE:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    rest_element_SUITE:end_per_testcase(TestCase, Config).


%% Tests ============================

create(Config) ->
    UrlSchema = get_schema_url(Config),
    Url1 = UrlSchema ++ "/connections/2",
    SourceId = 1, SourceOutput = 1, TargetId = 1, TargetInput = 2,
    Url1 = create_connection(UrlSchema, SourceId, SourceOutput, TargetId, TargetInput),
    Id = rest_utils:id_from_url(Url1),
    #{id := Id, source_id := SourceId, source_output := SourceOutput,
      target_id := TargetId, target_input := TargetInput, href := Url1} =
        rest_utils:get_json(Url1),

    Url2 = UrlSchema ++ "/connections/3",
    Url2 = create_connection(UrlSchema, SourceId, SourceOutput, TargetId, 1),
    #{connections := Connections} = rest_utils:get_json(UrlSchema),
    [#{target_input := Input1}, #{target_input := Input2}] = Connections,
    true = sets:from_list([Input1, Input2]) == sets:from_list([1, 2]).

delete(Config) ->
    UrlSchema = get_schema_url(Config),
    Url = create_connection(UrlSchema),
    rest_utils:delete_url(Url),
    rest_utils:get_json_fail(Url),
    #{connections := []} = rest_utils:get_json(UrlSchema).

update(Config) ->
    UrlSchema = get_schema_url(Config),
    Url = create_connection(UrlSchema),
    #{target_input := 2} = rest_utils:get_json(Url),
    update_connection(Url, 1, 1, 1, 3),
    #{target_input := 3} = rest_utils:get_json(Url).


%% Utils ============================

get_schema_url(Config) ->
    {url_schema, UrlSchema} = lists:keyfind(url_schema, 1, Config),
    UrlSchema.

create_connection(UrlSchema, SourceId, SourceOutput, TargetId, TargetInput) ->
    Json = create_json(SourceId, SourceOutput, TargetId, TargetInput),
    {ok, {{_Version, 200, _Reason}, Headers, _Body}} =
        httpc:request(post, {
            rest_utils:absolute_url(UrlSchema ++ "/connections"),
                [], "application/json", Json},
            [{autoredirect, false}], []),
    {_, Location} = lists:keyfind("location", 1, Headers),
    Location.

create_connection(UrlSchema) ->
    create_connection(UrlSchema, 1, 1, 1, 2).

update_connection(Url, SourceId, SourceOutput, TargetId, TargetInput) ->
    rest_utils:update_url(Url, create_json(SourceId, SourceOutput, TargetId, TargetInput)).

create_json(SourceId, SourceOutput, TargetId, TargetInput) ->
    jiffy:encode(#{source_id => SourceId, source_output => SourceOutput,
                   target_id => TargetId, target_input => TargetInput, vertices => []}).
