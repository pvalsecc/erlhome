%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2015 11:44 AM
%%%-------------------------------------------------------------------
-module(rest_schema_SUITE).

-include_lib("common_test/include/ct.hrl").

%% API
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([create/1, delete/1, update/1]).

-export([create_schema/1]).

all() ->
    [create, delete, update].

init_per_testcase(_TestCase, Config) ->
    {ok, AppStartList} = application:ensure_all_started(erlhome),
    {ok, AppStartList2} = application:ensure_all_started(inets),
    NewConfig = lists:keystore(app_start_list, 1, Config, {app_start_list,
            AppStartList ++ AppStartList2}),
    NewConfig.

end_per_testcase(_TestCase, Config) ->
    stop(?config(app_start_list, Config)),
    Config.

stop([]) ->
    ok;
stop([H|Rest]) ->
    ok = application:stop(H),
    stop(Rest).


%% Tests ============================

create(_Config) ->
    [] = get_schemas(),
    Url1 = "/schemas/1",
    Url1 = create_schema("toto"),
    #{name := "toto"} = rest_utils:get_json(Url1),

    Url2 = "/schemas/2",
    Url2 = create_schema("tutu"),
    [#{name := Name1}, #{name := Name2}] = get_schemas(),
    true = sets:from_list([Name1, Name2]) ==
        sets:from_list(["toto", "tutu"]).


delete(_Config) ->
    Url = create_schema("toto"),
    rest_utils:delete_url(Url),
    rest_utils:get_json_fail(Url),
    [] = get_schemas().

update(_Config) ->
    Url = create_schema("toto"),
    #{name := "toto"} = rest_utils:get_json(Url),
    update_schema(Url, "tutu"),
    #{name := "tutu"} = rest_utils:get_json(Url).

%% Utils ============================

get_schemas() ->
    rest_utils:get_json("/schemas").

create_schema(Name) ->
    Json = create_json(Name),
    %TODO: why is it not 201?
    {ok, {{_Version, 204, _Reason}, Headers, Body}} =
        httpc:request(post, {rest_utils:absolute_url("/schemas"), [],
            "application/json", Json},
            [{autoredirect, false}], []),
    "" = Body,
    {_, Location} = lists:keyfind("location", 1, Headers),
    Location.

update_schema(Url, Name) ->
    rest_utils:update_url(Url, create_json(Name)).

create_json(Name) ->
    "{\"name\":\"" ++ Name ++ "\"}".
