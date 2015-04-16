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

-export([create_schema/1, update_url/2, delete_url/1, get_json/1,
get_json_fail/1]).

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
    #{<<"name">> := <<"toto">>} = get_json(Url1),

    Url2 = "/schemas/2",
    Url2 = create_schema("tutu"),
    [#{<<"name">> := Name1}, #{<<"name">> := Name2}] = get_schemas(),
    true = sets:from_list([Name1, Name2]) ==
        sets:from_list([<<"toto">>, <<"tutu">>]).


delete(_Config) ->
    Url = create_schema("toto"),
    delete_url(Url),
    get_json_fail(Url),
    [] = get_schemas().

update(_Config) ->
    Url = create_schema("toto"),
    #{<<"name">> := <<"toto">>} = get_json(Url),
    update_schema(Url, "tutu"),
    #{<<"name">> := <<"tutu">>} = get_json(Url).

%% Utils ============================

get_schemas() ->
    get_json("/schemas").

get_json(Url) ->
    {ok, {{_Version, 200, _Reason}, _Headers, Body}} =
        httpc:request(get, {"http://localhost:8080" ++ Url, []}, [], []),
    jiffy:decode(Body, [return_maps]).

get_json_fail(Url) ->
    {ok, {{_Version, 404, _Reason}, _Headers, _Body}} =
        httpc:request(get, {"http://localhost:8080" ++ Url, []}, [], []).

delete_url(Url) ->
    {ok, {{_Version, 204, _Reason}, _Headers, Body}} =
        httpc:request(delete, {"http://localhost:8080" ++ Url, []}, [], []),
    "" = Body.

create_schema(Name) ->
    Json = create_json(Name),
    %TODO: why is it not 201?
    {ok, {{_Version, 204, _Reason}, Headers, Body}} =
        httpc:request(post, {"http://localhost:8080/schemas", [],
            "application/json", Json},
            [{autoredirect, false}], []),
    "" = Body,
    {_, Location} = lists:keyfind("location", 1, Headers),
    Location.

update_schema(Url, Name) ->
    update_url(Url, create_json(Name)).

create_json(Name) ->
    "{\"name\":\"" ++ Name ++ "\"}".

update_url(Url, Json) ->
    {ok, {{_Version, 204, _Reason}, _Headers, Body}} =
        httpc:request(put, {"http://localhost:8080" ++ Url, [],
            "application/json", Json},
            [{autoredirect, false}], []),
    "" = Body.
