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
-export([start_link/1, switch/2, control/3]).

-export([init/1, new_inputs/3, iterate_status/3]).

-record(state, {id :: integer(), status = false :: boolean()}).

-spec(start_link(Id :: integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Id) ->
    ehome_element:start_link(Id, ehome_switch, 0, 1, Id).

-spec(switch(Gate :: pid, Value :: boolean()) -> ok).
switch(Gate, Value) ->
    ehome_element:new_outputs(Gate, [Value]).

init(Id) ->
    gen_event:notify(status_notif, create_notif(Id, false)),
    #state{id = Id}.

new_inputs(_Inputs, _OldOutputs, _State) ->
    %no input => should not be called
    erlang:error(not_implemented).

iterate_status(Callback, Acc, #state{id = Id, status = Status}) ->
    Callback(#notif{type = switch, id = Id, value = Status}, Acc).

create_notif(Id, Value) ->
    #notif{type = switch, id = Id, value = Value}.

control(<<"switch">>, Message, #state{id = Id} = Inner)
        when is_boolean(Message) ->
    gen_event:notify(status_notif, create_notif(Id, Message)),
    {[Message], Inner#state{status = Message}};

control(Type, Message, _Inner) ->
    io:format("ehome_switch: un-supported message ~p/~p~n", [Type, Message]),
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTs

-include_lib("eunit/include/eunit.hrl").

switch_test() ->
    {ok, Events} = gen_event:start_link({local, status_notif}),
    {ok, Switch} = start_link(1),
    {ok, Relay} = ehome_relay:start_link(2),
    ehome_element:connect(Switch, 1, Relay, 1, 1),
    test_utils:wait_queues_empty([Switch, Relay]),
    [false] = ehome_element:get_inputs(Relay),
    switch(Switch, true),
    test_utils:wait_queues_empty([Switch, Relay]),
    [true] = ehome_element:get_inputs(Relay),
    gen_event:stop(Events).
