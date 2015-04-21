%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Apr 2015 8:08 AM
%%%-------------------------------------------------------------------
-module(iterate_status_SUITE).
-author("pvalsecc").

-include_lib("common_test/include/ct.hrl").
-include("../src/ehome_types.hrl").

%% API
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([test/1]).

all() ->
    [test].

init_per_testcase(_TestCase, Config) ->
    {ok, Events} = gen_event:start_link({local, change_notif}),
    {ok, Sup} = ehome_elements_sup:start_link(),
    [{events, Events}, {sup, Sup} | Config].

end_per_testcase(_TestCase, Config) ->
    gen_event:stop(get_config(events, Config)),
    ehome_elements_sup:stop(get_config(sup, Config)),
    Config.

test(Config) ->
    Sup = get_config(sup, Config),
    ehome_elements_sup:handle_event(Sup, {create,
        #element{id = 1, type = <<"switch">>}}),
    ehome_elements_sup:handle_event(Sup, {create,
        #element{id = 2, type = <<"relay">>}}),
    ehome_elements_sup:handle_event(Sup, {create,
        #connection{id = 3, source_id = 1, source_output = 1,
                    target_id = 2, target_input = 1}}),
    Actual = ehome_elements_sup:iterate_status(fun(Type, Id, Value, Acc) ->
        [{Type, Id, Value}|Acc]
    end, []),
    Expected = [
        {switch, 1, false},
        {relay, 2, false},
        {connection, 3, false}
    ],
    io:format("Actual ~p~n", [Actual]),
    true = sets:from_list(Actual) == sets:from_list(Expected).

get_config(Key, Config) ->
    {Key, Value} = lists:keyfind(Key, 1, Config),
    Value.
