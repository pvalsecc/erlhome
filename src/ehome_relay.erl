%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Apr 2015 8:05 AM
%%%-------------------------------------------------------------------
-module(ehome_relay).
-author("pvalsecc").

-behaviour(ehome_element).

%% API
-export([start_link/1]).

-export([init/1, new_inputs/3, iterate_status/2]).

-record(state, {
    status = false :: boolean(),
    id :: integer()
}).

start_link(Id) ->
    ehome_element:start_link(Id, ehome_relay, 1, 0, Id).


init(Id) ->
    #state{id = Id}.

new_inputs([Input], _OldOutputs, #state{id = Id} = State) ->
    io:format("Relay ~p: ~p~n", [Id, Input]),
    State#state{status = Input}.

iterate_status(Callback, #state{id = Id, status = Status}) ->
    Callback(relay, Id, Status).