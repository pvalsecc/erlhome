%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2015 8:42 AM
%%%-------------------------------------------------------------------
-author("pvalsecc").

-record(element, {
    id :: integer(),
    type :: binary(),
    config = #{} :: map(),
    x :: integer(),
    y :: integer()
}).

-record(vertice, {
    x :: integer(),
    y :: integer()
}).

-record(connection, {
    id :: integer(),
    source_id :: integer(),
    source_output :: integer(),
    target_id :: integer(),
    target_input :: integer(),
    vertices = [] :: [#vertice{}]
}).

-record(schema, {
    id :: integer(),
    name :: binary() | string(),
    elements = [] :: [#element{}],
    connections = [] :: [#connection{}]
}).

-record(notif, {
    type :: atom(),
    id :: integer(),
    value :: boolean()
}).

-type status_callback() :: fun((Notif :: #notif{}, Acc :: any()) -> any()).
