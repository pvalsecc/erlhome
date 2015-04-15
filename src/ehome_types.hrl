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
    config :: any(),
    x :: integer(),
    y :: integer()
}).

-record(connection, {
    source_id :: integer(),
    source_output :: integer(),
    target_id :: integer(),
    target_input :: integer()
}).

-record(schema, {
    id :: integer(),
    name :: binary(),
    elements = [] :: [#element{}],
    connections = [] :: [#connection{}]
}).
