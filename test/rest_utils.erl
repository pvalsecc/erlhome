%%%-------------------------------------------------------------------
%%% @author patrick
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Apr 2015 10:45
%%%-------------------------------------------------------------------
-module(rest_utils).
-author("patrick").

%% API
-export([get_json/1, get_json_fail/1, delete_url/1, update_url/2]).

get_json(Url) ->
    {ok, {{_Version, 200, _Reason}, _Headers, Body}} =
        httpc:request(get, {"http://localhost:8080" ++ Url, []}, [], []),
    decode_json(Body).

get_json_fail(Url) ->
    {ok, {{_Version, 404, _Reason}, _Headers, _Body}} =
        httpc:request(get, {"http://localhost:8080" ++ Url, []}, [], []).

delete_url(Url) ->
    {ok, {{_Version, 204, _Reason}, _Headers, Body}} =
        httpc:request(delete, {"http://localhost:8080" ++ Url, []}, [], []),
    "" = Body.

update_url(Url, Json) ->
    {ok, {{_Version, 204, _Reason}, _Headers, Body}} =
        httpc:request(put, {"http://localhost:8080" ++ Url, [],
            "application/json", Json},
            [{autoredirect, false}], []),
    "" = Body.

decode_json(Text) ->
    simplify_json(jiffy:decode(Text, [return_maps])).

simplify_json({<<K/binary>>, V}) ->
    {list_to_atom(simplify_json(K)), simplify_json(V)};
simplify_json({K, V}) ->
    {simplify_json(K), simplify_json(V)};
simplify_json(List) when is_list(List) ->
    lists:map(fun simplify_json/1, List);
simplify_json(Map) when is_map(Map) ->
    maps:from_list(simplify_json(maps:to_list(Map)));
simplify_json(<<Bytes/binary>>) ->
    binary_to_list(Bytes);
simplify_json(Json) ->
    Json.

%%%===================================================================
%%% Tests
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

simplify_json_test() ->
    Src = #{<<"toto">> => [1, "2", <<"3">>], <<"tutu">> => <<"value">>},
    #{toto := [1, "2", "3"], tutu := "value"} = simplify_json(Src).