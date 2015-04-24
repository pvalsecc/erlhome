%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Apr 2015 9:25 AM
%%%-------------------------------------------------------------------
-module(db_notifs_SUITE).
-author("pvalsecc").

-include_lib("common_test/include/ct.hrl").
-include("../src/ehome_types.hrl").

%% API
-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([elements/1, schema_deletion/1]).

all() ->
    [elements, schema_deletion].

init_per_testcase(_TestCase, Config) ->
    {ok, _Pid} = gen_event:start_link({local, change_notif}),
    {ok, _Pid2} = ehome_db:start_link(false),
    gen_event:add_sup_handler(change_notif, event_recorder, []),
    Config.

end_per_testcase(_TestCase, Config) ->
    ehome_db:stop(),
    gen_event:stop(change_notif),
    Config.

elements(_Config) ->
    SchemaId = ehome_db:create_schema(#schema{name = "schema"}),
    [] = event_recorder:get_events(change_notif),

    Element = #element{type = "test", x = 1, y = 2},
    ElementId = ehome_db:create_element(SchemaId, Element),
    ExpectedElement = Element#element{id = ElementId},
    ExpectedEvents1 = [{create, ExpectedElement}],
    ExpectedEvents1 = event_recorder:get_events(change_notif),

    UpdatedElement = ExpectedElement#element{x = 33},
    true = ehome_db:update_element(SchemaId, ElementId, UpdatedElement),
    ExpectedEvents2 = [{update, UpdatedElement, ExpectedElement}],
    ExpectedEvents2 = event_recorder:get_events(change_notif),

    true = ehome_db:delete_element(SchemaId, ElementId),
    ExpectedEvents3 = [{delete, UpdatedElement}],
    ExpectedEvents3 = event_recorder:get_events(change_notif).

schema_deletion(_Config) ->
    SchemaId = ehome_db:create_schema(#schema{name = "schema"}),

    Element = #element{type = "test", x = 1, y = 2},
    ElementId = ehome_db:create_element(SchemaId, Element),
    ExpectedElement = Element#element{id = ElementId},
    ExpectedEvents1 = [{create, ExpectedElement}],
    ExpectedEvents1 = event_recorder:get_events(change_notif),

    true = ehome_db:delete_schema(SchemaId),
    ExpectedEvents2 = [{delete, ExpectedElement}],
    ExpectedEvents2 = event_recorder:get_events(change_notif).
