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

-behaviour(ehome_element).

%% API
-export([start_link/3]).

-export([init/1, new_inputs/3, control/3]).

-record(state, {
    schema_id :: integer(),
    id :: integer(),
    delay = 2000 :: non_neg_integer(),
    waiting = false :: false | timer:tref()
}).

-spec(start_link(SchemaId :: integer(), Id :: integer(), Config :: map()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(SchemaId, Id, Config) ->
    ehome_element:start_link(SchemaId, Id, ?MODULE, 2, 1,
        {SchemaId, Id, Config}).

init({SchemaId, Id, Config}) ->
    Delay = maps:get(<<"delay">>, Config, 2000),
    {[false], set_desc(#state{delay = Delay, schema_id = SchemaId, id = Id})}.

new_inputs([_, true], [_, false], State) ->
    % reset
    maybe_cancel(State),
    {new_outputs, [false], State#state{waiting = false}};

new_inputs([true, _], [false, _],
        #state{delay = 0, waiting = false} = State) ->
    % start; special case for delay = 0
    maybe_cancel(State),
    {new_outputs, [true], State#state{waiting = false}};

new_inputs([true, _], [false, _], State) ->
    % start; trigger the timer
    maybe_cancel(State),
    start(State);

new_inputs(_NewInputs, _OldInputs, State) ->
    State.

control(config, #{<<"delay">> := Delay}, State) ->
    set_desc(State#state{delay = Delay});
control(timer_triggered, _, State) ->
    maybe_cancel(State),
    publish_status(false, State),
    {new_outputs, [true], State#state{waiting = false}};
control(Type, Message, _Inner) ->
    io:format("ehome_timer_gate: un-supported message ~p/~p~n",
        [Type, Message]),
    false.

set_desc(#state{schema_id = SchemaId, id = Id, delay = Delay} = State) ->
    ehome_dispatcher:publish([status, desc, SchemaId, Id], get_desc(Delay),
        true),
    State.

get_desc(Delay) when Delay < 1000 ->
    <<(integer_to_binary(Delay))/binary, "ms">>;
get_desc(Delay) ->
    <<(integer_to_binary(Delay div 1000))/binary, "s">>.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maybe_cancel(#state{waiting = false}) ->
    ok;
maybe_cancel(#state{waiting = TRef} = State) ->
    publish_status(false, State),
    timer:cancel(TRef).

start(#state{delay = Delay} = State) ->
    publish_status(true, State),
    {ok, TRef} = timer:apply_after(Delay,
        ehome_element, control, [self(), timer_triggered, undefined]),
    State#state{waiting = TRef}.

publish_status(Value, #state{schema_id = SchemaId, id = Id}) ->
    ehome_dispatcher:publish([status, timer, SchemaId, Id], Value, true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

-define(DELAY, 50).
-define(MARGIN, 10).

nominal_test() ->
    {ok, Timer} = start_link(1, 1, #{<<"delay">> => ?DELAY}),
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
    {ok, Timer} = start_link(1, 1, #{<<"delay">> => ?DELAY}),
    ehome_element:set_input(Timer, 1, true),
    ehome_element:set_input(Timer, 1, false),
    test_utils:wait_queue_empty(Timer),
    [false] = ehome_element:get_outputs(Timer),
    ehome_element:set_input(Timer, 2, true),
    ehome_element:set_input(Timer, 1, false),
    timer:sleep(?DELAY + ?MARGIN),
    [false] = ehome_element:get_outputs(Timer).
