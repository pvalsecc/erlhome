%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2015 11:44 AM
%%%-------------------------------------------------------------------
-module(rest_element_SUITE).

-include_lib("common_test/include/ct.hrl").

%% API
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([create/1, delete/1, update/1]).

all() ->
    [create, delete, update].

init_per_testcase(TestCase, Config) ->
    NewConfig = rest_schema_SUITE:init_per_testcase(TestCase, Config),
    UrlSchema = rest_schema_SUITE:create_schema("test"),
    lists:keystore(url_schema, 1, NewConfig, {url_schema, UrlSchema}).

end_per_testcase(TestCase, Config) ->
    rest_schema_SUITE:end_per_testcase(TestCase, Config).


%% Tests ============================

create(Config) ->
    UrlSchema = get_schema_url(Config),
    Url1 = UrlSchema ++ "/elements/1",
    Url1 = create_element(UrlSchema, "element1"),
    #{type := "element1", x := 1, y := 1} = rest_utils:get_json(Url1),

    Url2 = UrlSchema ++ "/elements/2",
    Url2 = create_element(UrlSchema, "element2"),
    #{elements := Elements} = rest_utils:get_json(UrlSchema),
    [#{type := Type1}, #{type := Type2}] = Elements,
    true = sets:from_list([Type1, Type2]) ==
        sets:from_list(["element1", "element2"]).

delete(Config) ->
    UrlSchema = get_schema_url(Config),
    Url = create_element(UrlSchema, "element1"),
    rest_utils:delete_url(Url),
    rest_utils:get_json_fail(Url),
    #{elements := []} = rest_utils:get_json(UrlSchema).

update(Config) ->
    UrlSchema = get_schema_url(Config),
    Url = create_element(UrlSchema, "element1"),
    #{type := "element1"} = rest_utils:get_json(Url),
    update_element(Url, "element2"),
    #{type := "element2"} = rest_utils:get_json(Url).


%% Utils ============================

get_schema_url(Config) ->
    {url_schema, UrlSchema} = lists:keyfind(url_schema, 1, Config),
    UrlSchema.

create_element(UrlSchema, Type) ->
    Json = create_json(Type),
    %TODO: why is it not 201?
    {ok, {{_Version, 204, _Reason}, Headers, Body}} =
        httpc:request(post, {
                "http://localhost:8080" ++ UrlSchema ++ "/elements",
                [], "application/json", Json},
            [{autoredirect, false}], []),
    "" = Body,
    {_, Location} = lists:keyfind("location", 1, Headers),
    Location.

update_element(Url, Type) ->
    rest_utils:update_url(Url, create_json(Type)).

create_json(Type) ->
    "{\"type\":\"" ++ Type ++ "\", \"x\":1, \"y\":1}".
