%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%% See http://ninenines.eu/docs/en/cowboy/HEAD/guide/ws_handlers/
%%% @end
%%% Created : 20. Apr 2015 3:46 PM
%%%-------------------------------------------------------------------
-module(ehome_ws_notifs).
-author("pvalsecc").

-include("ehome_types.hrl").

%% cowboy websocket API
-export([init/2, websocket_handle/3, websocket_info/3, terminate/3]).

-record(state, {}).

init(Req, _Opts) ->
    Self = self(),
    Self ! send_current_states,
    ehome_dispatcher:subscribe([status, all], Self, fun(Path, Value) ->
        Self ! {event, Path, Value}
    end),
    {cowboy_websocket, Req, #state{}}.
    %TODO: add 60s timeout and have the client ping every 30s

websocket_handle({text, Text}, Req, State) ->
    io:format("websocket_handle: ~p~n", [Text]),
    {ok, Req, State}.

websocket_info(send_current_states, Req, State) ->
    Content = ehome_elements_sup:iterate_status(fun build_message/2, []),
    {reply, Content, Req, State};

websocket_info({event, [status, Type, _SchemaId, Id], Value}, Req,
        State) ->
    {reply, build_message(Type, Id, Value), Req, State}.

build_message(Notif, Acc) ->
    [build_message(Notif) | Acc].

build_message(#status{type = Type, id = Id, value = Value}) ->
    build_message(Type, Id, Value).

build_message(Type, Id, Value) ->
    {text, jiffy:encode(#{type => Type, id => Id, value => Value})}.

terminate(_Reason, _Req, State) ->
    ehome_dispatcher:unsubscribe(self()),
    State.