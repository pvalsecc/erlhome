%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Apr 2015 8:45 AM
%%%-------------------------------------------------------------------
-module(test_utils).
-author("pvalsecc").

%% API
-export([wait_queue_empty/1, wait_queues_empty/1, dispatcher_env/1, mqtt_tree_env/1]).

wait_queues_empty(Pids) ->
    lists:foreach(fun wait_queue_empty/1, Pids).

wait_queue_empty(Pid) ->
    %get_inputs is synchronous => when it returns, the queue has been emptied.
    ehome_element:get_inputs(Pid),
    ok.

dispatcher_env(Test) ->
    ehome_dispatcher:start_link(),
    try
        Test()
    after
        ehome_dispatcher:stop()
    end.

mqtt_tree_env(Test) ->
    dispatcher_env(fun() ->
        PrevMqtt = application:get_env(erlhome, enable_mqtt),
        application:set_env(erlhome, enable_mqtt, false),
        ehome_mqtt_tree:start_link(),
        try
            Test()
        after
            gen_server:call(ehome_mqtt_tree, stop),
            application:set_env(erlhome, enable_mqtt, PrevMqtt)
        end
    end).
