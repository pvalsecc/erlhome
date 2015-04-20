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

-export([init/1, new_inputs/3]).

-record(state, {
    status = false :: boolean()
}).

start_link(Name) ->
    ehome_element:start_link(Name, ehome_relay, 1, 0, []).


init(_Args) ->
    #state{}.

new_inputs([Input], _OldOutputs, State) ->
    #state{status = Input}.
