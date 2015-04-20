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

%% cowboy websocket API
-export([init/2, websocket_handle/3, websocket_info/3]).

-record(state, {}).

init(Req, _Opts) ->
    self() ! send_current_states,
    %TODO: subscribe to gen_events for receiving stuff
    {cowboy_websocket, Req, #state{}, 60000}.
    %TODO: add to the client ping sending every 30 seconds

websocket_handle(Frame, Req, State) ->
    io:format("websocket_handle: ~p~n", [Frame]),
    {ok, Req, State}.

websocket_info(send_current_states, Req, State) ->
    %TODO: send current states instead of that:
    {reply, {text, <<"Hello World!">>}, Req, State};

websocket_info(Info, Req, State) ->
    %TODO: receive messages from the gen_events here and send messages to the
    %      client
    io:format("websocket_info: ~p~n", [Info]),
    {ok, Req, State}.
