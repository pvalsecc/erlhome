%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Apr 2015 7:48 AM
%%%-------------------------------------------------------------------
-module(ehome_switch).
-author("pvalsecc").

-include("ehome_types.hrl").

-behaviour(ehome_element).

%% API
-export([start_link/2, switch/2, control/3]).

-export([init/1, new_inputs/3, iterate_status/3]).

-record(state, {
    schema_id :: integer(),
    id :: integer(),
    status = false :: boolean()
}).

-spec(start_link(SchemaId :: integer(), Id :: integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(SchemaId, Id) ->
    ehome_element:start_link(SchemaId, Id, ehome_switch, 0, 1, {SchemaId, Id}).

-spec(switch(Gate :: pid(), Value :: boolean()) -> ok).
switch(Gate, Value) ->
    ehome_element:new_outputs(Gate, [Value]).

init({SchemaId, Id}) ->
    ehome_dispatcher:publish([status, switch, SchemaId, Id], false),
    {[false], #state{schema_id = SchemaId, id = Id}}.

new_inputs(_Inputs, _OldInputs, _State) ->
    %no input => should not be called
    erlang:error(not_implemented).

iterate_status(Callback, Acc, #state{id = Id, status = Status}) ->
    Callback(#status{type = switch, id = Id, value = Status}, Acc).

control(<<"switch">>, Message, Inner)
        when is_boolean(Message) ->
    new_value(Message, Inner);

control(<<"toggle">>, _Message, #state{status = OldValue} = Inner) ->
    new_value(not OldValue, Inner);

control(Type, Message, _Inner) ->
    io:format("ehome_switch: un-supported message ~p/~p~n", [Type, Message]),
    false.

new_value(Value, #state{schema_id = SchemaId, id = Id} = Inner) ->
    ehome_dispatcher:publish([status, switch, SchemaId, Id], Value),
    {new_outputs, [Value], Inner#state{status = Value}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTs

-include_lib("eunit/include/eunit.hrl").

switch_test() ->
    ehome_dispatcher:start_link(),
    {ok, Switch} = start_link(1, 1),
    {ok, Relay} = ehome_relay:start_link(1, 2),
    ehome_element:connect(Switch, 1, Relay, 1, 1),
    test_utils:wait_queues_empty([Switch, Relay]),
    [false] = ehome_element:get_inputs(Relay),
    switch(Switch, true),
    test_utils:wait_queues_empty([Switch, Relay]),
    [true] = ehome_element:get_inputs(Relay),
    ehome_dispatcher:stop().
