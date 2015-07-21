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
    {Host, _} = cowboy_req:peer(Req),
    lager:md([{desc, io_lib:format("client=~p", [Host])}]),
    {cowboy_websocket, Req, #state{}}.
    %TODO: add 60s timeout and have the client ping every 30s

websocket_handle({text, Text}, Req, State) ->
    {Command, Param} = parse_command(Text),
    handle_command(Command, Param),
    {ok, Req, State}.

websocket_info({event, Path, Value}, Req,
        State) ->
    {reply, build_message(Path, Value), Req, State}.

build_message([status, Type, _SchemaId, Id], Value) ->
    {text, jiffy:encode(#{type => Type, id => Id, value => Value})};
build_message(Path, Value) ->
    {text, jiffy:encode(#{path => Path, value => Value})}.

terminate(_Reason, _Req, State) ->
    ehome_dispatcher:unsubscribe(self()),
    State.

parse_command(Text) ->
    case binary:match(Text, <<" ">>) of
        {SplitPos, _} ->
            <<Command:SplitPos/binary, " ", Param/binary>> = Text,
            {Command, Param};
        nomatch ->
            {Text, <<>>}
    end.

handle_command(<<"subscribe">>, PathTxt) ->
    Path = ehome_utils:parse_path(PathTxt),
    Self = self(),
    lager:debug("subscribe to ~p", [Path]),
    ehome_dispatcher:subscribe(Path, Self, fun(ActualPath, Value) ->
        Self ! {event, ActualPath, Value}
    end);
handle_command(<<"unsubscribe">>, <<>>) ->
    lager:debug("unsubscribe all"),
    ehome_dispatcher:unsubscribe(self()).

%%%===================================================================
%%% Tests
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

parse_command_test() ->
    {<<"subscribe">>, <<"[status, all]">>} =
        parse_command(<<"subscribe [status, all]">>),
    {<<"unsubscribe">>, <<>>} = parse_command(<<"unsubscribe">>).
