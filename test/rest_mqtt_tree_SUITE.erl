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

expected(DeviceId, InstanceId, Value) ->
    Id = io_lib:format("~p", [[DeviceId, InstanceId, switch_binary, "level"]]),
    Desc = io_lib:format("~p/~p", [DeviceId, InstanceId]),
    #{id => lists:flatten(Id), desc => lists:flatten(Desc),
        name => lists:flatten(Desc), value => Value}.

switch_binary(_Config) ->
    ehome_mqtt_tree:fake_switch(2, 0, true),
    ehome_mqtt_tree:fake_switch(2, 1, false),
    ehome_mqtt_tree:fake_switch(2, 2, true),
    ehome_mqtt_tree:fake_switch(3, 0, false),
    Actual = rest_utils:get_json("/zwave/switch_binary"),
    Expected = [
        expected(2, 0, true),
        expected(2, 1, false),
        expected(2, 2, true),
        expected(3, 0, false)
    ],
    Expected = Actual.
