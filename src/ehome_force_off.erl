%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Apr 2015 2:49 PM
%%%-------------------------------------------------------------------
-module(ehome_force_off).
-author("pvalsecc").

-behaviour(ehome_element).

%% API
-export([start_link/2]).

-export([init/1, new_inputs/3, control/3]).

-record(state, {xor_value = false :: boolean()}).

start_link(SchemaId, Id) ->
    ehome_element:start_link(SchemaId, Id, ?MODULE, 2, 1, []).

init(_Args) ->
    {[false], #state{}}.

new_inputs([Main, true], [_, false], State) ->
    {new_outputs, [false], State#state{xor_value = Main}};
new_inputs([Main, _], _OldInputs, #state{xor_value = Xor} = State) ->
    {new_outputs, [Main xor Xor], State}.

control(Type, Message, _Inner) ->
    io:format("ehome_force_off: un-supported message ~p/~p~n",
        [Type, Message]),
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    {ok, Gate} = start_link(1, 1),
    test_utils:wait_queue_empty(Gate),
    [false] = ehome_element:get_outputs(Gate),
    ok = ehome_element:set_input(Gate, 1, true),
    test_utils:wait_queue_empty(Gate),
    [true] = ehome_element:get_outputs(Gate),

    ok = ehome_element:set_input(Gate, 2, true),
    ok = ehome_element:set_input(Gate, 2, false),
    test_utils:wait_queue_empty(Gate),
    [false] = ehome_element:get_outputs(Gate),

    ok = ehome_element:set_input(Gate, 1, false),
    test_utils:wait_queue_empty(Gate),
    [true] = ehome_element:get_outputs(Gate).


