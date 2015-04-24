%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Apr 2015 8:27 AM
%%%-------------------------------------------------------------------
-module(ehome_timer_gate).
-author("pvalsecc").

-include("ehome_types.hrl").

-behaviour(ehome_element).

%% API
-export([start_link/2]).

-export([init/1, new_inputs/3, iterate_status/3, control/3]).

-record(state, {
    delay = 2000 :: non_neg_integer(),
    waiting = false :: false | pid()
}).

-spec(start_link(Id :: integer(), Delay :: non_neg_integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Id, Delay) ->
    ehome_element:start_link(Id, ?MODULE, 2, 1, Delay).

init(Delay) ->
    {[false], #state{delay = Delay}}.

new_inputs([false, false], _OldOutputs, State) ->
    State;

new_inputs([true, false], _OldOutputs,
        #state{delay = 0, waiting = false} = State) ->
    % special case for delay = 0
    {new_outputs, [true], State};

new_inputs([true, false], _OldOutputs,
        #state{delay = Delay, waiting = false} = State) ->
    % triggering the timer
    {ok, TRef} = timer:apply_after(Delay,
        ehome_element, new_outputs, [self(), [true]]),
    State#state{waiting = TRef};

new_inputs([true, false], _OldOutputs, State) ->
    % already triggered
    State;

new_inputs([_, true], _OldOutputs, #state{waiting = TRef} = State) ->
    % reset
    maybe_cancel(TRef),
    {new_outputs, [false], State#state{waiting = false}}.

iterate_status(_Callback, Acc, _Inner) ->
    Acc.

control(Type, Message, _Inner) ->
    io:format("ehome_timer_gate: un-supported message ~p/~p~n",
        [Type, Message]),
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maybe_cancel(false) ->
    ok;
maybe_cancel(TRef) ->
    timer:cancel(TRef).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

-define(DELAY, 50).
-define(MARGIN, 10).

nominal_test() ->
    {ok, Timer} = start_link(1, ?DELAY),
    ehome_element:set_input(Timer, 1, true),
    test_utils:wait_queue_empty(Timer),
    [false] = ehome_element:get_outputs(Timer),
    ehome_element:set_input(Timer, 1, false),
    test_utils:wait_queue_empty(Timer),
    [false] = ehome_element:get_outputs(Timer),
    timer:sleep(?DELAY + ?MARGIN),
    [true] = ehome_element:get_outputs(Timer),
    ehome_element:set_input(Timer, 2, true),
    test_utils:wait_queue_empty(Timer),
    [false] = ehome_element:get_outputs(Timer),
    ehome_element:set_input(Timer, 2, false),
    test_utils:wait_queue_empty(Timer),
    [false] = ehome_element:get_outputs(Timer).

reset_before_trigger_test() ->
    {ok, Timer} = start_link(1, ?DELAY),
    ehome_element:set_input(Timer, 1, true),
    ehome_element:set_input(Timer, 1, false),
    test_utils:wait_queue_empty(Timer),
    [false] = ehome_element:get_outputs(Timer),
    ehome_element:set_input(Timer, 2, true),
    ehome_element:set_input(Timer, 1, false),
    timer:sleep(?DELAY + ?MARGIN),
    [false] = ehome_element:get_outputs(Timer).
