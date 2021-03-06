%%%-------------------------------------------------------------------
%%% @author patrick
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Apr 2015 20:44
%%%-------------------------------------------------------------------
-module(ehome_d_flipflop).
-author("patrick").

-behaviour(ehome_element).

%% API
-export([init/1, new_inputs/3, control/3, start_link/2]).

start_link(SchemaId, Id) ->
    ehome_element:start_link(SchemaId, Id, ehome_d_flipflop, 2, 1, []).

init(_Args) ->
    {[false], undefined}.

new_inputs([D, true], [_, false], State) ->
    {new_outputs, [D], State};
new_inputs([_D, _Clock], _OldInputs, State) ->
    State.

control(Type, Message, _Inner) ->
    io:format("ehome_d_flipflop: un-supported message ~p/~p~n",
        [Type, Message]),
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

nominal_test() ->
    {ok, FlipFlop} = start_link(1, 1),
    [false] = ehome_element:get_outputs(FlipFlop),
    ehome_element:set_input(FlipFlop, 1, true),
    test_utils:wait_queue_empty(FlipFlop),
    [false] = ehome_element:get_outputs(FlipFlop),
    ehome_element:set_input(FlipFlop, 2, true),
    test_utils:wait_queue_empty(FlipFlop),
    [true] = ehome_element:get_outputs(FlipFlop),
    ehome_element:set_input(FlipFlop, 2, false),
    [true] = ehome_element:get_outputs(FlipFlop),
    ehome_element:set_input(FlipFlop, 1, false),
    ehome_element:set_input(FlipFlop, 2, false),
    [true] = ehome_element:get_outputs(FlipFlop),
    ehome_element:set_input(FlipFlop, 2, true),
    test_utils:wait_queue_empty(FlipFlop),
    [false] = ehome_element:get_outputs(FlipFlop).
