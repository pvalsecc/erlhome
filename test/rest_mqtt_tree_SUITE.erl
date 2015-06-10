%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jun 2015 8:44 AM
%%%-------------------------------------------------------------------
-module(rest_mqtt_tree_SUITE).
-author("pvalsecc").
-include_lib("common_test/include/ct.hrl").

%% API
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([switch_binary/1]).

all() -> [switch_binary].

init_per_testcase(TestCase, Config) ->
    rest_schema_SUITE:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    rest_schema_SUITE:end_per_testcase(TestCase, Config).


%% Tests ============================

add_fake(DeviceId, InstanceId) ->
    Topic = lists:flatten(io_lib:format(
        "zwave/get/devices/~w/instances/~w/commandClasses/37/data/level",
        [DeviceId, InstanceId])),
    io:format("~p~n", [Topic]),
    gen_server:cast(ehome_mqtt_tree, {from_mqtt, Topic, <<1:8, 1:8>>}).

expected(DeviceId, InstanceId) ->
    Id = io_lib:format("~p", [[DeviceId, InstanceId, switch_binary, "level"]]),
    Desc = io_lib:format("~p/~p", [DeviceId, InstanceId]),
    #{id => lists:flatten(Id), desc => lists:flatten(Desc)}.

switch_binary(_Config) ->
    add_fake(2, 0),
    add_fake(2, 1),
    add_fake(2, 2),
    add_fake(3, 0),
    Actual = rest_utils:get_json("/zwave/switch_binary"),
    Expected = [
        expected(2, 0),
        expected(2, 1),
        expected(2, 2),
        expected(3, 0)
    ],
    Expected = Actual.
