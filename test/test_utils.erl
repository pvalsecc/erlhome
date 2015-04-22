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
-export([wait_queue_empty/1, wait_queues_empty/1]).

wait_queues_empty(Pids) ->
    lists:foreach(fun wait_queue_empty/1, Pids).

wait_queue_empty(Pid) ->
    %get_inputs is synchronous => when it returns, the queue has been emptied.
    ehome_element:get_inputs(Pid),
    ok.
