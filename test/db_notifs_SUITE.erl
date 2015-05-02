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
    ehome_dispatcher:start_link(),
    {ok, _Pid2} = ehome_db:start_link(false),
    dispatcher_recorder:start_link(),
    dispatcher_recorder:subscribe([db, all]),
    Config.

end_per_testcase(_TestCase, Config) ->
    ehome_db:stop(),
    dispatcher_recorder:stop(),
    ehome_dispatcher:stop(),
    Config.

elements(_Config) ->
    SchemaId = ehome_db:create_schema(#schema{name = "schema"}),
    [] = dispatcher_recorder:get_events(),

    Element = #element{type = "and", x = 1, y = 2},
    ElementId = ehome_db:create_element(SchemaId, Element),
    ExpectedElement = Element#element{id = ElementId},
    ExpectedEvents1 = [{[db, create, element, SchemaId, ElementId], ExpectedElement}],
    ExpectedEvents1 = dispatcher_recorder:get_events(),

    UpdatedElement = ExpectedElement#element{x = 33},
    true = ehome_db:update_element(SchemaId, ElementId, UpdatedElement),
    ExpectedEvents2 = [
        {[db, update, element, SchemaId, ElementId], {UpdatedElement, ExpectedElement}}
    ],
    ExpectedEvents2 = dispatcher_recorder:get_events(),

    true = ehome_db:delete_element(SchemaId, ElementId),
    ExpectedEvents3 = [{[db, delete, element, SchemaId, ElementId], UpdatedElement}],
    ExpectedEvents3 = dispatcher_recorder:get_events().

schema_deletion(_Config) ->
    SchemaId = ehome_db:create_schema(#schema{name = "schema"}),

    Element = #element{type = "and", x = 1, y = 2},
    ElementId = ehome_db:create_element(SchemaId, Element),
    ExpectedElement = Element#element{id = ElementId},
    ExpectedEvents1 = [{[db, create, element, SchemaId, ElementId], ExpectedElement}],
    ExpectedEvents1 = dispatcher_recorder:get_events(),

    true = ehome_db:delete_schema(SchemaId),
    ExpectedEvents2 = [{[db, delete, element, SchemaId, ElementId], ExpectedElement}],
    ExpectedEvents2 = dispatcher_recorder:get_events().
