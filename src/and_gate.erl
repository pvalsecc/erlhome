%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Apr 2015 2:37 PM
%%%-------------------------------------------------------------------
-module(and_gate).
-author("pvalsecc").

-behaviour(logic_gate).

%% API
-export([start_link/1, new_inputs/3]).

%% and_gate callbacks
-export([init/1]).

-spec(start_link(Name :: term()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name) ->
    logic_gate:start_link(Name, ?MODULE, 2, 1, []).

init([]) ->
    undefined.

new_inputs(Gate, [A, B], undefined) ->
    logic_gate:new_outputs(Gate, [A and B], undefined),
    undefined.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTs

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    {ok, And} = and_gate:start_link("AND"),
    logic_gate:set_input(And, 1, true).

