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
-export([init/2, websocket_handle/3, websocket_info/3, handle_event/2]).

-record(state, {}).

init(Req, _Opts) ->
    self() ! send_current_states,
    ehome_event_forwarder:register(status_notif, ?MODULE, self()),
    {cowboy_websocket, Req, #state{}}.
    %TODO: add 60s timeout and have the client ping every 30s

handle_event(Target, Event) ->
    Target ! {event, Event}.

websocket_handle({text, Text}, Req, State) ->
    io:format("websocket_handle: ~p~n", [Text]),
    {ok, Req, State}.

websocket_info(send_current_states, Req, State) ->
    Content = ehome_elements_sup:iterate_status(fun build_message/2, []),
    {reply, Content, Req, State};

websocket_info({event, Notif}, Req,
        State) ->
    {reply, build_message(Notif), Req, State}.

build_message(Notif, Acc) ->
    [build_message(Notif) | Acc].

build_message(#notif{type = Type, id = Id, value = Value}) ->
    {text, jiffy:encode(#{type => Type, id => Id, value => Value})}.

