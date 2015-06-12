%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Jun 2015 8:30 AM
%%%-------------------------------------------------------------------
-module(ehome_pmap).
-author("pvalsecc").

%% API
-export([new/2, close/1, set/3, get/2, remove/2]).

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
    #pmap{db = Db};
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

-spec get(#pmap{}, Key :: any()) -> {#pmap{}, Value :: any()}.
get(#pmap{cache = Cache} = Pmap, Key) ->
    case maps:get(Key, Cache, undefined) of
        undefined -> get_db(Pmap, Key);
        Value -> {Pmap, Value}
    end.

-spec remove(#pmap{}, Key :: any()) -> #pmap{}.
remove(#pmap{db = Db, cache = Cache} = Pmap, Key) ->
    remove_db(Db, Key),
    NewCache = maps:remove(Key, Cache),
    Pmap#pmap{cache = NewCache}.

set_db(undefined, _Key, _Value) ->
    ok;
set_db(Db, Key, Value) ->
    dets:insert(Db, {Key, Value}).

get_db(#pmap{db = undefined, cache = Cache} = Pmap, Key) ->
    {Pmap#pmap{cache = maps:put(Key, undefined, Cache)}, undefined};
get_db(#pmap{db = Db, cache = Cache} = Pmap, Key) ->
    Value = case dets:lookup(Db, Key) of
        [{_, Val}] -> Val;
        [] -> undefined
    end,
    {Pmap#pmap{cache = maps:put(Key, Value, Cache)}, Value}.

remove_db(undefined, _Key) ->
    ok;
remove_db(Db, Key) ->
    ok = dets:delete(Db, Key).

%%%===================================================================
%%% Tests
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

read_check(Map) ->
    {Map2, 3} = get(Map, a),
    {Map3, 2} = get(Map2, b),
    {Map6, undefined} = get(Map3, c),
    Map6.

basic_check(Map) ->
    {Map2, undefined} = get(Map, a),
    Map3 = set(Map2, a, 1),
    Map4 = set(Map3, b, 2),
    Map5 = set(Map4, a, 3),
    Map6 = read_check(Map5),
    Map7 = remove(Map6, a),
    {Map8, undefined} = get(Map7, a),
    Map9 = remove(Map8, z),
    ok = close(Map9).

non_persistent_test() ->
    Map = new("test", false),
    basic_check(Map).

persistent_test() ->
    Map = new("test", true),
    dets:delete_all_objects(Map#pmap.db),
    basic_check(Map),

    Map2 = new("test", true),
    {Map3, undefined} = get(Map2, a),
    {Map4, 2} = get(Map3, b),
    ok = close(Map4).
