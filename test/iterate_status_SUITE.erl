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
    {ok, _Dispatcher} = ehome_dispatcher:start_link(),
    {ok, Status} = gen_event:start_link({local, status_notif}),
    {ok, Sup} = ehome_elements_sup:start_link(),
    [{events, [Status]}, {sup, Sup} | Config].

end_per_testcase(_TestCase, Config) ->
    ehome_elements_sup:stop(rest_utils:get_config(sup, Config)),
    lists:map(fun gen_event:stop/1, rest_utils:get_config(events, Config)),
    ehome_dispatcher:stop(),
    Config.

test(_Config) ->
    ehome_elements_sup:handle_event([db, create, element, 1, 1],
        #element{id = 1, type = <<"switch">>}),
    ehome_elements_sup:handle_event([db, create, element, 1, 2],
        #element{id = 2, type = <<"relay">>}),
    ehome_elements_sup:handle_event([db, create, connection, 1, 3],
        #connection{id = 3, source_id = 1, source_output = 1,
                    target_id = 2, target_input = 1}),
    Actual = ehome_elements_sup:iterate_status(fun(Notif, Acc) ->
        [Notif|Acc]
    end, []),
    Expected = [
        #notif{type = switch, id = 1, value = false},
        #notif{type = relay, id = 2, value = false},
        #notif{type = connection, id = 3, value = false}
    ],
    io:format("Actual ~p~n", [Actual]),
    true = sets:from_list(Actual) == sets:from_list(Expected).
