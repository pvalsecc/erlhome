%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Apr 2015 11:06 AM
%%%-------------------------------------------------------------------
-module(ehome_edge_gate).
-author("pvalsecc").

-behaviour(ehome_element).

%% API
-export([up_start_link/1, down_start_link/1, both_start_link/1]).

-export([init/1, new_inputs/3, iterate_status/3, control/3]).

-record(state, {
    trigger :: up|down|both
}).

-define(DELAY, 100).

-spec(up_start_link(Id :: integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
up_start_link(Id) ->
    start_link(Id, up).

-spec(down_start_link(Id :: integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
down_start_link(Id) ->
    start_link(Id, down).

-spec(both_start_link(Id :: integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
both_start_link(Id) ->
    start_link(Id, both).

start_link(Id, Trigger) ->
    ehome_element:start_link(Id, ?MODULE, 1, 1, Trigger).

init(Trigger) ->
    {[false], #state{trigger = Trigger}}.

new_inputs([Input], _OldOutputs, State) ->
    case is_edge(Input, State) of
        true ->
            % triggering the timer
            timer:apply_after(?DELAY,
                ehome_element, new_outputs, [self(), [false]]),
            {new_outputs, [true], State};
        false ->
            State
    end.

iterate_status(_Callback, Acc, _Inner) ->
    Acc.

control(Type, Message, _Inner) ->
    io:format("ehome_edge_gate: un-supported message ~p/~p~n",
        [Type, Message]),
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_edge(Input, #state{trigger = up}) ->
    Input;
is_edge(Input, #state{trigger = down}) ->
    not Input;
is_edge(_Input, #state{trigger = both}) ->
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

-define(MARGIN, 10).

up_test() ->
    {ok, Events} = gen_event:start_link({local, status_notif}),
    {ok, Up} = up_start_link(1),
    {ok, Memory} = ehome_timer_gate:start_link(2, #{<<"delay">> => 0}),
    ok = ehome_element:connect(Up, 1, Memory, 1, 3),
    test_utils:wait_queues_empty([Up, Memory]),
    [false] = ehome_element:get_outputs(Up),
    [false] = ehome_element:get_outputs(Memory),

    ehome_element:set_input(Up, 1, true),
    test_utils:wait_queues_empty([Up, Memory]),
    [true] = ehome_element:get_outputs(Memory),
    timer:sleep(?DELAY + ?MARGIN),
    [false] = ehome_element:get_outputs(Up),

    gen_event:stop(Events).

down_test() ->
    {ok, Events} = gen_event:start_link({local, status_notif}),
    {ok, Down} = down_start_link(1),
    {ok, Memory} = ehome_timer_gate:start_link(2, #{<<"delay">> => 0}),
    ok = ehome_element:connect(Down, 1, Memory, 1, 3),
    test_utils:wait_queues_empty([Down, Memory]),
    [false] = ehome_element:get_outputs(Down),
    [false] = ehome_element:get_outputs(Memory),

    ehome_element:set_input(Down, 1, true),
    test_utils:wait_queues_empty([Down, Memory]),
    [false] = ehome_element:get_outputs(Down),
    [false] = ehome_element:get_outputs(Memory),

    ehome_element:set_input(Down, 1, false),
    test_utils:wait_queues_empty([Down, Memory]),
    [true] = ehome_element:get_outputs(Memory),
    timer:sleep(?DELAY + ?MARGIN),
    [false] = ehome_element:get_outputs(Down),

    gen_event:stop(Events).

both_test() ->
    {ok, Events} = gen_event:start_link({local, status_notif}),
    {ok, Both} = both_start_link(1),
    {ok, Memory} = ehome_timer_gate:start_link(2, #{<<"delay">> => 0}),
    ok = ehome_element:connect(Both, 1, Memory, 1, 3),
    test_utils:wait_queues_empty([Both, Memory]),
    [false] = ehome_element:get_outputs(Both),
    [false] = ehome_element:get_outputs(Memory),

    ehome_element:set_input(Both, 1, true),
    test_utils:wait_queues_empty([Both, Memory]),
    [true] = ehome_element:get_outputs(Memory),
    timer:sleep(?DELAY + ?MARGIN),
    [false] = ehome_element:get_outputs(Both),

    ehome_element:set_input(Memory, 2, true),
    ehome_element:set_input(Memory, 2, false),
    test_utils:wait_queues_empty([Both, Memory]),
    [false] = ehome_element:get_outputs(Both),
    [false] = ehome_element:get_outputs(Memory),

    ehome_element:set_input(Both, 1, false),
    test_utils:wait_queues_empty([Both, Memory]),
    [true] = ehome_element:get_outputs(Memory),
    timer:sleep(?DELAY + ?MARGIN),
    [false] = ehome_element:get_outputs(Both),

    gen_event:stop(Events).
