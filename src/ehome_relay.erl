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

-include("ehome_types.hrl").

-behaviour(ehome_element).

%% API
-export([start_link/1, control/3]).

-export([init/1, new_inputs/3, iterate_status/3]).

-record(state, {
    status = false :: boolean(),
    id :: integer()
}).

start_link(Id) ->
    ehome_element:start_link(Id, ehome_relay, 1, 0, Id).


init(Id) ->
    #state{id = Id}.

new_inputs([Input], _OldOutputs, #state{id = Id} = State) ->
    gen_event:notify(status_notif, create_notif(Id, Input)),
    State#state{status = Input}.

iterate_status(Callback, Acc, #state{id = Id, status = Status}) ->
    Callback(create_notif(Id, Status), Acc).

control(Type, Message, _Inner) ->
    io:format("ehome_relay: un-supported message ~p/~p~n", [Type, Message]),
    false.

create_notif(Id, Status) ->
    #notif{type = relay, id = Id, value = Status}.
