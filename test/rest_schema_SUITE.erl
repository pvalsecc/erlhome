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
    application:stop(H),
    stop(Rest).


%% Tests ============================

create(_Config) ->
    [] = get_schemas(),
    Url1 = "/schemas/1",
    Url1 = create_schema("toto"),
    #{<<"name">> := <<"toto">>} = get_schema(Url1),

    Url2 = "/schemas/2",
    Url2 = create_schema("tutu"),
    [#{<<"name">> := Name1}, #{<<"name">> := Name2}] = get_schemas(),
    true = sets:from_list([Name1, Name2]) ==
        sets:from_list([<<"toto">>, <<"tutu">>]).


delete(_Config) ->
    Url = create_schema("toto"),
    delete_schema(Url),
    get_schema_fail(Url),
    [] = get_schemas().

update(_Config) ->
    Url = create_schema("toto"),
    #{<<"name">> := <<"toto">>} = get_schema(Url),
    update_schema(Url, "tutu"),
    #{<<"name">> := <<"tutu">>} = get_schema(Url).

%% Utils ============================

get_schemas() ->
    {ok, {{_Version, 200, _Reason}, _Headers, Body}} =
        httpc:request(get, {"http://localhost:8080/schemas", []}, [], []),
    jiffy:decode(Body, [return_maps]).

get_schema(Url) ->
    {ok, {{_Version, 200, _Reason}, _Headers, Body}} =
        httpc:request(get, {"http://localhost:8080" ++ Url, []}, [], []),
    jiffy:decode(Body, [return_maps]).

get_schema_fail(Url) ->
    {ok, {{_Version, 404, _Reason}, _Headers, _Body}} =
        httpc:request(get, {"http://localhost:8080" ++ Url, []}, [], []).

delete_schema(Url) ->
    {ok, {{_Version, 204, _Reason}, _Headers, Body}} =
        httpc:request(delete, {"http://localhost:8080" ++ Url, []}, [], []).

create_schema(Name) ->
    Json = "{\"name\":\"" ++ Name ++ "\"}",
    %TODO: why is it not 201?
    {ok, {{_Version, 204, _Reason}, Headers, Body}} =
        httpc:request(post, {"http://localhost:8080/schemas", [],
            "application/json", Json},
            [{autoredirect, false}], []),
    "" = Body,
    {_, Location} = lists:keyfind("location", 1, Headers),
    Location.

update_schema(Url, Name) ->
    Json = "{\"name\":\"" ++ Name ++ "\"}",
    {ok, {{_Version, 204, _Reason}, Headers, Body}} =
        httpc:request(put, {"http://localhost:8080" ++ Url, [],
            "application/json", Json},
            [{autoredirect, false}], []),
    "" = Body.
