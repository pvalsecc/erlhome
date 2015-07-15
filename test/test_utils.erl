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
-export([wait_queue_empty/1, wait_queues_empty/1, dispatcher_env/1,
    mqtt_tree_env/1, app_env_env/3]).

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

app_env_env(EnvName, EnvValue, Test) ->
    Orig = application:get_env(erlhome, EnvName),

    application:set_env(erlhome, EnvName, EnvValue),
    try
        Test()
    after
        application:set_env(erlhome, EnvName, Orig)
    end.

mqtt_tree_env(Test) ->
    dispatcher_env(fun() ->
        app_env_env(enable_mqtt, false, fun() ->
            ehome_mqtt_tree:start_link(),
            try
                Test()
            after
                gen_server:call(ehome_mqtt_tree, stop)
            end
        end)
    end).
