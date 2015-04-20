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
-export([wait_queue_empty/1]).

wait_queue_empty(Pid) ->
    case erlang:process_info(Pid, message_queue_len) of
        {message_queue_len, 0} ->
            ok;
        _ ->
            timer:sleep(1),
            wait_queue_empty(Pid)
    end.
