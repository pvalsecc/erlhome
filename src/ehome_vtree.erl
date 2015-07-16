%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jun 2015 3:36 PM
%%%-------------------------------------------------------------------
-module(ehome_vtree).
-author("pvalsecc").

%% API
-export([new/0, iterate_subs/3, set_value/3, get_value/2, noop_iterator/2, create_filter_iterator/1, dumper/2]).

-record(node, {
    value = undefined :: any(),
    subs = #{} :: #{any() => #node{}}
}).

-type iterator() ::
fun(({start, Key :: any(), Value :: any()} | {stop, Key :: any()},
Acc::any()) ->
    {SubIterator :: iterator(), NextAcc :: any()}).

-export_type([iterator/0]).

-spec new() -> #node{}.
new() ->
    #node{}.


-spec set_value(Path :: [any()], Value :: any(), Root :: #node{}) ->
    {NewRoot :: #node{}, boolean()}.
set_value([], Value, #node{value = Value} = Root) ->
    {Root, false};
set_value([], Value, Root) ->
    {Root#node{value = Value}, true};
set_value([H|Rest], Value, #node{subs = Subs} = Root) ->
    Sub = maps:get(H, Subs, #node{}),
    {NewSub, Changed} = set_value(Rest, Value, Sub),
    {Root#node{subs = maps:put(H, NewSub, Subs)}, Changed}.

-spec get_value(Path :: [any()], Root :: #node{}) -> any() | undefined.
get_value([], #node{value = Value}) ->
    Value;
get_value([Name|Rest], #node{subs = Subs}) ->
    case maps:get(Name, Subs, undefined) of
        undefined -> undefined;
        Sub -> get_value(Rest, Sub)
    end.


-spec iterate_subs(iterator(), Acc :: any(), Root :: #node{}) -> Acc2 :: any().
iterate_subs(Iterator, Acc, #node{subs = Subs}) ->
    iterate_subs_impl(Iterator, Acc, Subs).

iterate(Iterator, Acc, Name, #node{value = Value, subs = Subs}) ->
    {SubIterator, Acc1} =  Iterator({start, Name, Value}, Acc),
    Acc2 = iterate_subs_impl(ehome_utils:maybe(SubIterator, Iterator), Acc1, Subs),
    {_, Acc3} = Iterator({stop, Name}, Acc2),
    Acc3.

iterate_subs_impl(Iterator, Acc, #{} = Subs) ->
    iterate_subs_impl(Iterator, Acc, maps:to_list(Subs));
iterate_subs_impl(_Iterator, Acc, []) ->
    Acc;
iterate_subs_impl(Iterator, Acc, [{Name, Sub} | Rest]) ->
    Acc1 = iterate(Iterator, Acc, Name, Sub),
    iterate_subs_impl(Iterator, Acc1, Rest).

noop_iterator(_, Acc) ->
    {undefined, Acc}.

all_iterator({start, Name, Value}, {CurPath, PrevFilters, [], Results}) ->
    io:format("name=~p~n", [Name]),
    NextPath = [Name|CurPath],
    NextResults = [{lists:reverse(NextPath), Value} | Results],
    {undefined, {NextPath, PrevFilters, [], NextResults}};
all_iterator({stop, Name}, {[Name | CurPath], PrevFilters, FRest, Results}) ->
    {undefined, {CurPath, PrevFilters, FRest, Results}}.

apply_filter(Name, Value, {CurPath, PrevFilters, [all], Results}) ->
    NextPath = [Name|CurPath],
    NextResults = [{lists:reverse(NextPath), Value} | Results],
    {fun all_iterator/2, {NextPath, [all|PrevFilters], [], NextResults}};
apply_filter(Name, Value, {CurPath, PrevFilters, [FHead], Results}) ->
    NextPath = [Name|CurPath],
    NextResults = case FHead of
                      any -> [{lists:reverse(NextPath), Value} | Results];
                      Name -> [{lists:reverse(NextPath), Value} | Results];
                      _ -> Results
                  end,
    {fun noop_iterator/2,
        {NextPath, [FHead|PrevFilters], [], NextResults}};
apply_filter(Name, _Value, {CurPath, PrevFilters, [FHead|FRest], Results}) ->
    Acc = {[Name|CurPath], [FHead|PrevFilters], FRest, Results},
    case FHead of
        any -> {undefined, Acc};
        Name -> {undefined, Acc};
        _ -> {fun ehome_vtree:noop_iterator/2, Acc}
    end.

-spec create_filter_iterator([all | any | any()]) ->
    {IteratorFun :: iterator(), IteratorAcc :: any()}.
create_filter_iterator(Filter) ->
    {
        fun({start, Name, Value}, Acc) ->
                apply_filter(Name, Value, Acc);
           ({stop, Name},
                    {[Name | CurPath], [FHead|PrevFilters], FRest, Results}) ->
                {undefined, {CurPath, PrevFilters, [FHead|FRest], Results}}
        end,
        {[], [], Filter, []}
    }.

dumper({start, Name, undefined}, Indent) ->
    io:format("~s~p~n", [Indent, Name]),
    {fun dumper/2, "    " ++ Indent};
dumper({start, Name, Value}, Indent) ->
    io:format("~s~p = ~p~n", [Indent, Name, Value]),
    {fun dumper/2, "    " ++ Indent};
dumper({stop, _Name}, [_, _, _, _ | Acc]) ->
    {undefined, Acc}.

%%%===================================================================
%%% Tests
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

set_value_test() ->
    {T1, true} = set_value([a,b,c], 1, #node{}),
    {T2, true} = set_value([a,b,d], 2, T1),
    {_, false} = set_value([a,b,d], 2, T2),
    #node{subs =
        #{a := #node{subs =
            #{b := #node{subs =
                #{c := #node{value = 1},
                    d := #node{value = 2}
                }
            }}
        }}
    } = T2.

simple_iterate_sub_test() ->
    {T1, _} = set_value([a,b], 1, #node{}),
    {T2, _} = set_value([a,d], 2, T1),
    {T3, _} = set_value([e], 3, T2),
    Calls = iterate(fun(Call, Acc) -> {undefined, [Call | Acc]} end,
        [], root, T3),
    Expected = [
        {start, root, undefined},
        {start, a, undefined},
        {start, b, 1},
        {stop, b},
        {start, d, 2},
        {stop, d},
        {stop, a},
        {start, e, 3},
        {stop, e},
        {stop, root}
    ],
    Expected = lists:reverse(Calls).

change_iterate_sub_test() ->
    {T1, _} = set_value([a,b], 1, #node{}),
    {T2, _} = set_value([a,d], 2, T1),
    {T3, _} = set_value([e], 3, T2),
    Calls = iterate(fun(Call, Acc) -> {fun noop_iterator/2, [Call | Acc]} end,
        [], root,
        T3),
    Expected = [
        {start, root, undefined},
        {stop, root}
    ],
    Expected = lists:reverse(Calls).

get_test() ->
    {Root, _} = set_value([a,b], 1, #node{}),
    1 = get_value([a,b], Root),
    undefined = get_value([a,c], Root),
    undefined = get_value([c,f,e], Root).

build_test_tree() ->
    lists:foldl(fun({K, V}, Acc) ->
                    {Ret, true} = set_value(K, V, Acc),
                    Ret
                end, new(),
    [
        {[a,b] ,2},
        {[a,d] ,3},
        {[a,d,e] ,4},
        {[f] ,5},
        {[g,b] ,6}
    ]).

filter_iterator_test() ->
    Root = build_test_tree(),
    Filter = [r, any, b],
    {FilterIt, FilterAcc} = create_filter_iterator(Filter),
    {[], [], Filter, Actual} = iterate(FilterIt, FilterAcc, r, Root),
    Expected = [
        {[r, a, b], 2},
        {[r, g, b], 6}
    ],
    Expected = lists:reverse(Actual).

filter_iterator_single_test() ->
    Root = build_test_tree(),
    Filter = [r, a, b],
    {FilterIt, FilterAcc} = create_filter_iterator(Filter),
    {[], [], Filter, Actual} = iterate(FilterIt, FilterAcc, r, Root),
    Expected = [
        {[r, a, b],2}
    ],
    Expected = lists:reverse(Actual).

filter_iterator_all_test() ->
    Root = build_test_tree(),
    Filter = [r, a, all],
    {FilterIt, FilterAcc} = create_filter_iterator(Filter),
    {[], [], Filter, Actual} = iterate(FilterIt, FilterAcc, r, Root),
    Expected = [
        {[r, a, b], 2},
        {[r, a, d], 3},
        {[r, a, d, e], 4}
    ],
    Expected = lists:reverse(Actual).

