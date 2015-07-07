%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%% Persitent map.
%%% @end
%%% Created : 12. Jun 2015 8:30 AM
%%%-------------------------------------------------------------------
-module(ehome_pmap).
-author("pvalsecc").

%% API
-export([new/2, close/1, set/3, get/2, remove/2, get_map/1]).

-record(pmap, {
    db,
    cache = #{} :: #{}
}).


-spec new(Name :: string(), Persistent :: boolean()) -> any().
new(Name, true) ->
    {ok, Db} = dets:open_file(Name, [
        {keypos, 1},
        {type, set}
    ]),
    Cache = maps:from_list(dets:traverse(Db, fun(E) -> {continue, E} end)),
    #pmap{db = Db, cache = Cache};
new(_, false) ->
    #pmap{}.

-spec close(#pmap{}) -> ok.
close(#pmap{db = undefined}) ->
    ok;
close(#pmap{db = Db}) ->
    ok = dets:close(Db).

-spec set(#pmap{}, Key :: any(), Value :: any()) -> #pmap{}.
set(#pmap{db = Db, cache = Cache} = Pmap, Key, Value) ->
    set_db(Db, Key, Value),
    Pmap#pmap{cache = maps:put(Key, Value, Cache)}.

-spec get(#pmap{}, Key :: any()) -> Value :: any().
get(#pmap{cache = Cache}, Key) ->
    maps:get(Key, Cache, undefined).

-spec get_map(#pmap{}) -> map().
get_map(#pmap{cache = Cache}) ->
    Cache.

-spec remove(#pmap{}, Key :: any()) -> #pmap{}.
remove(#pmap{db = Db, cache = Cache} = Pmap, Key) ->
    remove_db(Db, Key),
    NewCache = maps:remove(Key, Cache),
    Pmap#pmap{cache = NewCache}.

set_db(undefined, _Key, _Value) ->
    ok;
set_db(Db, Key, Value) ->
    dets:insert(Db, {Key, Value}).

remove_db(undefined, _Key) ->
    ok;
remove_db(Db, Key) ->
    ok = dets:delete(Db, Key).

%%%===================================================================
%%% Tests
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

read_check(Map) ->
    3 = get(Map, a),
    2 = get(Map, b),
    undefined = get(Map, c),
    #{a := 3, b := 2} = get_map(Map).

basic_check(Map) ->
    undefined = get(Map, a),
    Map3 = set(Map, a, 1),
    Map4 = set(Map3, b, 2),
    Map5 = set(Map4, a, 3),
    read_check(Map5),
    Map6 = remove(Map5, a),
    undefined = get(Map6, a),
    Map8 = remove(Map6, z),
    ok = close(Map8).

non_persistent_test() ->
    Map = new("test", false),
    basic_check(Map).

persistent_test() ->
    Map = new("test", true),
    dets:delete_all_objects(Map#pmap.db),
    basic_check(Map),

    Map2 = new("test", true),
    undefined = get(Map2, a),
    2 = get(Map2, b),
    ok = close(Map2).
