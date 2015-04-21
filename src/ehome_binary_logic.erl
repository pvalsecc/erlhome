%%%-------------------------------------------------------------------
%%% @author patrick
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Apr 2015 23:31
%%%-------------------------------------------------------------------
-module(ehome_binary_logic).
-author("patrick").

-behaviour(ehome_element).

%% API
-export([and_start_link/1, or_start_link/1, xor_start_link/1,
    iterate_status/3]).

%% ehome_element callbacks
-export([init/1, new_inputs/3]).

-record(state, {
    function :: fun((boolean(), boolean()) -> boolean)
}).

-spec(and_start_link(Id :: integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
and_start_link(Id) ->
    start_link(Id, fun(A, B) -> A and B end).

-spec(or_start_link(Id :: integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
or_start_link(Id) ->
    start_link(Id, fun(A, B) -> A or B end).

-spec(xor_start_link(Id :: integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
xor_start_link(Id) ->
    start_link(Id, fun(A, B) -> A xor B end).

start_link(Id, Fun) ->
    ehome_element:start_link(Id, ?MODULE, 2, 1, [Fun]).

init([Fun]) ->
    #state{function = Fun}.

new_inputs([A, B], _OldOutputs, #state{function = Fun} = State) ->
    {new_outputs, [Fun(A, B)], State}.

iterate_status(_Callback, Acc, _Inner) ->
    Acc.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTs

-include_lib("eunit/include/eunit.hrl").

and_test() ->
    {ok, And} = and_start_link(1),
    [false] = ehome_element:get_outputs(And),
    ehome_element:set_input(And, 1, true),
    test_utils:wait_queue_empty(And),
    [false] = ehome_element:get_outputs(And),
    ehome_element:set_input(And, 2, true),
    test_utils:wait_queue_empty(And),
    [true] = ehome_element:get_outputs(And).

or_test() ->
    {ok, Or} = or_start_link(1),
    [false] = ehome_element:get_outputs(Or),
    ehome_element:set_input(Or, 1, true),
    test_utils:wait_queue_empty(Or),
    [true] = ehome_element:get_outputs(Or),
    ehome_element:set_input(Or, 2, true),
    test_utils:wait_queue_empty(Or),
    [true] = ehome_element:get_outputs(Or).

xor_test() ->
    {ok, Or} = xor_start_link(1),
    [false] = ehome_element:get_outputs(Or),
    ehome_element:set_input(Or, 1, true),
    test_utils:wait_queue_empty(Or),
    [true] = ehome_element:get_outputs(Or),
    ehome_element:set_input(Or, 2, true),
    test_utils:wait_queue_empty(Or),
    [false] = ehome_element:get_outputs(Or).

connection_test() ->
    {ok, And} = and_start_link(1),
    {ok, Or} = or_start_link(2),
    ok = ehome_element:connect(And, 1, Or, 1, 1),
    [false] = ehome_element:get_outputs(Or),
    ehome_element:set_input(And, 2, true),
    test_utils:wait_queue_empty(And),
    test_utils:wait_queue_empty(Or),
    [false] = ehome_element:get_outputs(Or),
    ehome_element:set_input(And, 1, true),
    test_utils:wait_queue_empty(And),
    test_utils:wait_queue_empty(Or),
    [true] = ehome_element:get_outputs(Or).

multi_connection_test() ->
    {ok, And} = and_start_link(1),
    {ok, Or1} = or_start_link(2),
    {ok, Or2} = or_start_link(3),
    ok = ehome_element:connect(And, 1, Or1, 1, 1),
    ok = ehome_element:connect(And, 1, Or2, 2, 2),
    [false] = ehome_element:get_outputs(Or1),
    [false] = ehome_element:get_outputs(Or2),
    ehome_element:set_input(And, 2, true),
    test_utils:wait_queue_empty(And),
    test_utils:wait_queue_empty(Or1),
    test_utils:wait_queue_empty(Or2),
    [false] = ehome_element:get_outputs(Or1),
    [false] = ehome_element:get_outputs(Or2),
    ehome_element:set_input(And, 1, true),
    test_utils:wait_queue_empty(And),
    test_utils:wait_queue_empty(Or1),
    test_utils:wait_queue_empty(Or2),
    [true] = ehome_element:get_outputs(Or1),
    [true] = ehome_element:get_outputs(Or2).
