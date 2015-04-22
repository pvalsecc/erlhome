%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2015 11:44 AM
%%%-------------------------------------------------------------------
-module(rest_control_SUITE).

-include_lib("common_test/include/ct.hrl").

%% API
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([switch/1]).

all() ->
    [switch].

init_per_testcase(TestCase, Config) ->
    NewConfig = rest_schema_SUITE:init_per_testcase(TestCase, Config),
    UrlSchema = rest_schema_SUITE:create_schema("test"),
    UrlSwitch = rest_element_SUITE:create_element(UrlSchema, "switch"),
    IdSwitch = rest_utils:id_from_url(UrlSwitch),
    UrlRelay = rest_element_SUITE:create_element(UrlSchema, "relay"),
    IdRelay = rest_utils:id_from_url(UrlRelay),
    UrlConnection = rest_connection_SUITE:create_connection(UrlSchema,
        IdSwitch, 1, IdRelay, 1),
    IdConnection = rest_utils:id_from_url(UrlConnection),
    gen_event:add_sup_handler(status_notif, event_recorder, []),
    [{id_switch, IdSwitch}, {id_relay, IdRelay}, {id_connection, IdConnection} |
        NewConfig].

end_per_testcase(TestCase, Config) ->
    rest_schema_SUITE:end_per_testcase(TestCase, Config).


%% Tests ============================

switch(Config) ->
    IdSwitch = rest_utils:get_config(id_switch, Config),
    IdRelay = rest_utils:get_config(id_relay, Config),
    IdConnection = rest_utils:get_config(id_connection, Config),
    send_control("switch", IdSwitch, true),
    Expected = [
        {notif, switch, IdSwitch, true},
        {notif, connection, IdConnection, true},
        {notif, relay, IdRelay, true}],
    Actual = event_recorder:get_events(status_notif),
    ct:log("Actual = ~p", [Actual]),
    true = sets:from_list(Actual) == sets:from_list(Expected).

send_control(Type, Id, Message) ->
    Json = jiffy:encode(Message),
    Url = rest_utils:absolute_url("/controls/" ++ Type ++ "/" ++
        integer_to_list(Id)),
    {ok, {{_Version, 204, _Reason}, _Headers, Body}} =
        httpc:request(put, {Url, [], "application/json", Json},
            [{autoredirect, false}], []),
    "" = Body,
    ok.
