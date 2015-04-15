%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2015 8:40 AM
%%%-------------------------------------------------------------------
-module(ehome_db).
-author("pvalsecc").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get_schemas/0, get_schema/1, create_schema/1, update_schema/2,
    delete_schema/1]).
-export([create_element/2, update_element/3, delete_element/2,
    get_element/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-include("ehome_types.hrl").

-record(state, {
    schemas = [] :: [#schema{}],
    next_schema_id :: integer(),
    next_element_id :: integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(stop() -> ok).
stop() ->
    gen_server:cast(?SERVER, stop).

-spec(get_schemas() -> [#schema{}]).
get_schemas() ->
    gen_server:call(?SERVER, {get_schemas}).

-spec(get_schema(SchemaId :: integer()) -> #schema{}).
get_schema(SchemaId) ->
    gen_server:call(?SERVER, {get_schema, SchemaId}).

-spec(create_schema(#schema{}) -> SchemaId :: integer()).
create_schema(#schema{id = undefined} = Schema) ->
    gen_server:call(?SERVER, {create_schema, Schema}).

-spec(get_element(SchemaId :: integer(), ElementId :: integer()) -> #element{}).
get_element(SchemaId, ElementId) ->
    gen_server:call(?SERVER, {get_element, SchemaId, ElementId}).

-spec(update_schema(SchemaId :: integer(), #schema{}) -> boolean()).
update_schema(SchemaId, #schema{id = undefined} = Schema) ->
    gen_server:call(?SERVER, {update_schema, Schema#schema{id = SchemaId}});
update_schema(SchemaId, #schema{id = SchemaId} = Schema) ->
    gen_server:call(?SERVER, {update_schema, Schema}).

-spec(delete_schema(integer()) -> true|false).
delete_schema(SchemaId) ->
    gen_server:call(?SERVER, {delete_schema, SchemaId}).

-spec(create_element(SchemaId :: integer(), Element :: #element{}) ->
    ElementId :: integer() | false).
create_element(SchemaId, #element{id = undefined} = Element) ->
    gen_server:call(?SERVER, {create_element, SchemaId, Element}).

-spec(update_element(SchemaId :: integer(), ElementId :: integer(),
        Element :: #element{}) -> true|false).
update_element(SchemaId, ElementId, #element{id = undefined} = Element) ->
    gen_server:call(?SERVER,
        {update_element, SchemaId, Element#element{id = ElementId}});
update_element(SchemaId, ElementId, #element{id = ElementId} = Element) ->
    gen_server:call(?SERVER, {update_element, SchemaId, Element}).

-spec(delete_element(SchemaId :: integer(), ElementId :: integer()) ->
    true|false).
delete_element(SchemaId, ElementId) ->
    gen_server:call(?SERVER, {delete_element, SchemaId, ElementId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    %TODO: load from disk
    {ok, #state{next_schema_id = 1, next_element_id = 1}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
        State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call({get_schemas}, _From, #state{schemas = Schemas} = State) ->
    {reply, Schemas, State};
handle_call({get_schema, Id}, _From, #state{schemas = Schemas} = State) ->
    {reply, get_schema(Id, Schemas), State};
handle_call({create_schema, Schema}, _From, State) ->
    {Id, NewState} = add_schema(Schema, State),
    {reply, Id, NewState};
handle_call({update_schema, Schema}, _From, State) ->
    {Result, NewState} = update_schema_impl(Schema, State),
    {reply, Result, NewState};
handle_call({delete_schema, Id}, _From, State) ->
    {Result, NewState} = delete_schema(Id, State),
    {reply, Result, NewState};
handle_call({get_element, SchemaId, ElementId}, _From, State) ->
    {reply, get_element(SchemaId, ElementId, State), State};
handle_call({create_element, SchemaId, Element}, _From, State) ->
    {Id, NewState} = add_element(SchemaId, Element, State),
    {reply, Id, NewState};
handle_call({update_element, SchemaId, Element}, _From, State) ->
    {Result, NewState} = update_element_impl(SchemaId, Element, State),
    {reply, Result, NewState};
handle_call({delete_element, SchemaId, ElementId}, _From, State) ->
    {Result, NewState} = delete_element(SchemaId, ElementId, State),
    {reply, Result, NewState}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(stop, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
        State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
        Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

add_schema(Schema, #state{next_schema_id = Id, schemas = Schemas} = State) ->
    NewSchema = Schema#schema{id = Id},
    {Id, State#state{next_schema_id = Id + 1, schemas = [NewSchema | Schemas]}}.

get_schema(Id, Schemas) ->
    lists:keyfind(Id, #schema.id, Schemas).

update_schema_impl(#schema{id = Id} = Schema, #state{schemas = Schemas} =
    State) ->
    NewSchemas = lists:keyreplace(Id, #schema.id, Schemas, Schema),
    {true, State#state{schemas=NewSchemas}}.

delete_schema(Id, #state{schemas = Schemas} = State) ->
    case lists:keydelete(Id, #schema.id, Schemas) of
        Schemas -> {false, State};
        NewSchemas -> {true, State#state{schemas = NewSchemas}}
    end.

get_element(SchemaId, ElementId, State) ->
    {Ret, State} = modify_schema_elements(SchemaId, fun(Elements) ->
        {lists:keyfind(ElementId, #element.id, Elements), Elements}
    end, State),
    Ret.

add_element(SchemaId, Element, #state{next_element_id = Id} = State) ->
    NewElement = Element#element{id = Id},
    modify_schema_elements(SchemaId, fun(Elements) ->
        {Id, [NewElement | Elements]}
    end, State#state{next_element_id = Id + 1}).

update_element_impl(SchemaId, #element{id = ElementId} = Element, State) ->
    modify_schema_elements(SchemaId, fun(Elements) ->
        {true, lists:keyreplace(ElementId, #element.id, Elements, Element)}
    end, State).

delete_element(SchemaId, ElementId, State) ->
    modify_schema_elements(SchemaId, fun(Elements) ->
        {true, lists:keydelete(ElementId, #element.id, Elements)}
    end, State).

modify_schema(SchemaId, Fun, #state{schemas = Schemas} = State) ->
    case lists:keyfind(SchemaId, #schema.id, Schemas) of
        false ->
            {false, State};
        Schema ->
            {Ret, NewSchema} = Fun(Schema),
            {Ret, State#state{
                schemas = lists:keyreplace(SchemaId, #schema.id, Schemas,
                    NewSchema)
            }}
    end.

modify_schema_elements(SchemaId, Fun, State) ->
    modify_schema(SchemaId, fun
        (#schema{elements = Elements} = Schema) ->
            {Ret, NewElements} = Fun(Elements),
            {Ret, Schema#schema{elements = NewElements}}
    end, State).

%%%===================================================================
%%% Tests
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

add_schema_test() ->
    NextId = 12,
    State = #state{next_schema_id = NextId},
    Schema = #schema{name = "toto"},
    ExpectedSchema = Schema#schema{id = NextId},
    ExpectedState = State#state{next_schema_id = NextId + 1,
                                schemas = [ExpectedSchema]},
    {NextId, ExpectedState} = add_schema(Schema, State).

add_element_test() ->
    NextSchemaId = 12,
    NextElementId = 22,
    State = #state{next_schema_id = NextSchemaId,
        next_element_id = NextElementId},
    Schema = #schema{name = "toto"},
    {SchemaId, State2} = add_schema(Schema, State),
    Element = #element{type = "test"},
    {ElementId, State3} = add_element(SchemaId, Element, State2),
    ElementId = NextElementId,
    ExpectedState = State#state{schemas = [Schema#schema{id = SchemaId, elements
    = [
        Element#element{id = ElementId}
    ]}], next_schema_id = SchemaId + 1, next_element_id = ElementId + 1},
    io:format("Expected = ~w~nActual = ~w~n", [ExpectedState, State3]),
    ExpectedState = State3.

update_element_test() ->
    NextSchemaId = 12,
    NextElementId = 22,
    State = #state{next_schema_id = NextSchemaId,
        next_element_id = NextElementId},
    Schema = #schema{name = "toto"},
    {SchemaId, State2} = add_schema(Schema, State),
    Element = #element{type = "test"},
    {ElementId, State3} = add_element(SchemaId, Element, State2),
    {true, _State4} = update_element_impl(SchemaId,
        Element#element{id = ElementId, type = "test2"}, State3).

schema_test() ->
    {ok, _PId} = start_link(),
    [] = get_schemas(),
    Schema = #schema{name="toto"},
    1 = Id = create_schema(Schema),
    NewSchema = Schema#schema{id = Id},
    [NewSchema] = get_schemas(),
    NewSchema = get_schema(Id),
    Renamed = NewSchema#schema{name = "titi"},
    true = update_schema(Id, Renamed),
    Renamed = get_schema(Id),
    false = delete_schema(Id + 1),
    [Renamed] = get_schemas(),
    true = delete_schema(Id),
    [] = get_schemas(),
    stop().

element_test() ->
    {ok, _PId} = start_link(),
    Schema = #schema{name = "toto"},
    SchemaId = create_schema(Schema),
    Element = #element{type = "test"},

    ElementId = create_element(SchemaId, Element),
    Element2 = Element#element{id = ElementId},
    Schema2 = Schema#schema{
        id = SchemaId,
        elements = [Element2]
    },
    Schema2 = get_schema(SchemaId),

    Element3 = Element2#element{type = "test2"},
    true = update_element(SchemaId, ElementId, Element3),
    Schema3 = Schema2#schema{elements = [Element3]},
    Schema3 = get_schema(SchemaId),
    Element3 = get_element(SchemaId, ElementId),

    delete_element(SchemaId, ElementId),
    Schema4 = Schema3#schema{elements = []},
    Schema4 = get_schema(SchemaId),
    false = get_element(SchemaId, ElementId).
