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

-behaviour(ehome_element).

%% API
-export([start_link/1, switch/2]).

-export([init/1, new_inputs/3]).


-spec(start_link(Name :: term()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name) ->
    ehome_element:start_link(Name, ehome_switch, 0, 1, []).

-spec(switch(Gate :: pid, Value :: boolean()) -> ok).
switch(Gate, Value) ->
    ehome_element:new_outputs(Gate, [Value]).

init(_Args) ->
    undefined.

new_inputs(_Inputs, _OldOutputs, _State) ->
    %no input => should not be called
    erlang:error(not_implemented).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTs

-include_lib("eunit/include/eunit.hrl").

switch_test() ->
    {ok, Switch} = start_link("switch"),
    {ok, Relay} = ehome_relay:start_link("relay"),
    ehome_element:connect(Switch, 1, Relay, 1),
    [false] = ehome_element:get_inputs(Relay),
    switch(Switch, true),
    test_utils:wait_queue_empty(Switch),
    test_utils:wait_queue_empty(Relay),
    [true] = ehome_element:get_inputs(Relay).