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
-export([start_link/2]).

-export([init/1, new_inputs/3, iterate_status/3, control/3]).

-record(state, {
    status = false :: boolean(),
    schema_id :: integer(),
    id :: integer()
}).

-spec(start_link(SchemaId :: integer(), Id :: integer()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(SchemaId, Id) ->
    ehome_element:start_link(SchemaId, Id, ehome_relay, 1, 0, {SchemaId, Id}).


init({SchemaId, Id}) ->
    {[], #state{schema_id = SchemaId, id = Id}}.

new_inputs([Input], _OldOutputs,
        #state{schema_id = SchemaId, id = Id} = State) ->
    ehome_dispatcher:publish([status, relay, SchemaId, Id], Input),
    State#state{status = Input}.

iterate_status(Callback, Acc, #state{id = Id, status = Status}) ->
    Callback(create_notif(Id, Status), Acc).

control(Type, Message, _Inner) ->
    io:format("ehome_relay: un-supported message ~p/~p~n", [Type, Message]),
    false.

create_notif(Id, Status) ->
    #status{type = relay, id = Id, value = Status}.
