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
-export([start_link/1, switch/2]).

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
    #state{id = Id}.

new_inputs(_Inputs, _OldOutputs, _State) ->
    %no input => should not be called
    erlang:error(not_implemented).

iterate_status(Callback, Acc, #state{id = Id, status = Status}) ->
    Callback(#notif{type = switch, id = Id, value = Status}, Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTs

-include_lib("eunit/include/eunit.hrl").

switch_test() ->
    {ok, Switch} = start_link(1),
    {ok, Relay} = ehome_relay:start_link(2),
    ehome_element:connect(Switch, 1, Relay, 1, 1),
    [false] = ehome_element:get_inputs(Relay),
    switch(Switch, true),
    test_utils:wait_queue_empty(Switch),
    test_utils:wait_queue_empty(Relay),
    timer:sleep(10), %TODO: understand why...
    [true] = ehome_element:get_inputs(Relay).
