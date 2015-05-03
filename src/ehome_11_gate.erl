%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Apr 2015 8:40 AM
%%%-------------------------------------------------------------------
-module(ehome_11_gate).
-author("pvalsecc").

-behaviour(ehome_element).

%% API
-export([not_start_link/2]).

%% ehome_element callbacks
-export([init/1, new_inputs/3, iterate_status/3, control/3]).

-record(state, {
    function :: fun((boolean()) -> boolean)
}).

-spec(not_start_link(SchemaId :: integer(), Id :: integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
not_start_link(SchemaId, Id) ->
    start_link(SchemaId, Id, fun(A) -> not A end).

start_link(SchemaId, Id, Fun) ->
    ehome_element:start_link(SchemaId, Id, ?MODULE, 1, 1, [Fun]).

init([Fun]) ->
    {[true], #state{function = Fun}}.

new_inputs([A], _OldOutputs, #state{function = Fun} = State) ->
    {new_outputs, [Fun(A)], State}.

iterate_status(_Callback, Acc, _Inner) ->
    Acc.

control(Type, Message, _Inner) ->
    io:format("ehome_11_gate: un-supported message ~p/~p~n",
        [Type, Message]),
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTs

-include_lib("eunit/include/eunit.hrl").

not_test() ->
    {ok, Not} = not_start_link(1, 1),
    [true] = ehome_element:get_outputs(Not),
    ehome_element:set_input(Not, 1, true),
    test_utils:wait_queue_empty(Not),
    [false] = ehome_element:get_outputs(Not),
    ehome_element:set_input(Not, 1, false),
    test_utils:wait_queue_empty(Not),
    [true] = ehome_element:get_outputs(Not).
