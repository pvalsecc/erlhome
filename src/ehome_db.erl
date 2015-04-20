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
-export([start_link/0, stop/0]).
-export([get_schemas/0, get_schema/1, create_schema/1, update_schema/2,
    delete_schema/1]).
-export([create_element/2, update_element/3, delete_element/2,
    get_element/2]).
-export([create_connection/2, update_connection/3, delete_connection/2,
    get_connection/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-include("ehome_types.hrl").

-record(state, {
    schemas = [] :: [#schema{}],
    next_schema_id :: integer(),
    next_sub_id :: integer()
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

%% Schemas

-spec(get_schemas() -> [#schema{}]).
get_schemas() ->
    gen_server:call(?SERVER, {get_schemas}).

-spec(get_schema(SchemaId :: integer()) -> #schema{}).
get_schema(SchemaId) ->
    gen_server:call(?SERVER, {get_schema, SchemaId}).

-spec(create_schema(#schema{}) -> SchemaId :: integer()).
create_schema(#schema{id = undefined} = Schema) ->
    gen_server:call(?SERVER, {create_schema, Schema}).

-spec(update_schema(SchemaId :: integer(), #schema{}) -> boolean()).
update_schema(SchemaId, #schema{id = undefined} = Schema) ->
    gen_server:call(?SERVER, {update_schema, Schema#schema{id = SchemaId}});
update_schema(SchemaId, #schema{id = SchemaId} = Schema) ->
    gen_server:call(?SERVER, {update_schema, Schema}).

-spec(delete_schema(integer()) -> true|false).
delete_schema(SchemaId) ->
    gen_server:call(?SERVER, {delete_schema, SchemaId}).

%% Elements

-spec(get_element(SchemaId :: integer(), ElementId :: integer()) -> #element{}).
get_element(SchemaId, ElementId) ->
    gen_server:call(?SERVER,
        {get_sub, SchemaId, ElementId, fun modify_schema_elements/3}).

-spec(create_element(SchemaId :: integer(), Element :: #element{}) ->
    ElementId :: integer() | false).
create_element(SchemaId, #element{id = undefined} = Element) ->
    gen_server:call(?SERVER,
        {create_sub, SchemaId, fun modify_schema_elements/3,
            fun(Id) -> Element#element{id = Id} end}).

-spec(update_element(SchemaId :: integer(), ElementId :: integer(),
        Element :: #element{}) -> true|false).
update_element(SchemaId, ElementId, #element{id = undefined} = Element) ->
    update_element(SchemaId, ElementId, Element#element{id = ElementId});
update_element(SchemaId, ElementId, #element{id = ElementId} = Element) ->
    gen_server:call(?SERVER,
        {update_sub, SchemaId, ElementId, Element,
            fun modify_schema_elements/3}).

-spec(delete_element(SchemaId :: integer(), ElementId :: integer()) ->
    true|false).
delete_element(SchemaId, ElementId) ->
    gen_server:call(?SERVER,
        {delete_sub, SchemaId, ElementId, fun modify_schema_elements/3}).

%% Connections

-spec(get_connection(SchemaId :: integer(), ConnectionId :: integer()) ->
    #connection{}).
get_connection(SchemaId, ConnectionId) ->
    gen_server:call(?SERVER,
        {get_sub, SchemaId, ConnectionId,
            fun modify_schema_connections/3}).

-spec(create_connection(SchemaId :: integer(), Connection :: #connection{}) ->
    ConnectionId :: integer()).
create_connection(SchemaId, #connection{id = undefined} = Connection) ->
    gen_server:call(?SERVER,
        {create_sub, SchemaId, fun modify_schema_connections/3,
            fun(Id) -> Connection#connection{id = Id} end}).

-spec(update_connection(SchemaId :: integer(), ConnectionId :: integer(),
        Connection :: #connection{}) -> true|false).
update_connection(SchemaId, ConnectionId, #connection{id = undefined} =
    Connection) ->
    update_connection(SchemaId, ConnectionId,
        Connection#connection{id = ConnectionId});
update_connection(SchemaId, ConnectionId,
        #connection{id = ConnectionId} = Connection) ->
    gen_server:call(?SERVER,
        {update_sub, SchemaId, ConnectionId, Connection,
            fun modify_schema_connections/3}).

-spec(delete_connection(SchemaId :: integer(), ConnectionId :: integer()) ->
    true|false).
delete_connection(SchemaId, ConnectionId) ->
    gen_server:call(?SERVER,
        {delete_sub, SchemaId, ConnectionId, fun modify_schema_connections/3}).


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
    {ok, #state{next_schema_id = 1, next_sub_id = 1}}.

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

handle_call({get_sub, SchemaId, SubId, Modifier}, _From, State) ->
    {reply, get_sub(SchemaId, SubId, State, Modifier), State};
handle_call({create_sub, SchemaId, Modifier, SubFactory}, _From,
        State) ->
    {Id, NewState} = create_sub(SchemaId, State, Modifier, SubFactory),
    {reply, Id, NewState};
handle_call({update_sub, SchemaId, SubId, Sub, Modifier}, _From, State) ->
    {Result, NewState} = update_sub(SchemaId, SubId, Sub, State, Modifier),
    {reply, Result, NewState};
handle_call({delete_sub, SchemaId, SubId, Modifier}, _From, State) ->
    {Result, NewState} = delete_sub(SchemaId, SubId, State, Modifier),
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
    {true, State#state{schemas = NewSchemas}}.

delete_schema(Id, #state{schemas = Schemas} = State) ->
    case lists:keydelete(Id, #schema.id, Schemas) of
        Schemas -> {false, State};
        NewSchemas -> {true, State#state{schemas = NewSchemas}}
    end.


get_sub(SchemaId, SubId, State, Modifier) ->
    {Ret, State} = Modifier(SchemaId, fun(Subs) ->
        {lists:keyfind(SubId, #element.id, Subs), Subs}
    end, State),
    Ret.

create_sub(SchemaId, #state{next_sub_id = Id} = State,
        Modifier, SubFactory) ->
    NewSub = SubFactory(Id),
    Modifier(SchemaId, fun(Subs) ->
        gen_event:notify(change_notif, {create, NewSub}),
        {Id, [NewSub | Subs]}
    end, State#state{next_sub_id = Id + 1}).

update_sub(SchemaId, SubId, Sub, State, Modifier) ->
    Modifier(SchemaId, fun(Subs) ->
        PrevSub = lists:keyfind(SubId, #element.id, Subs),
        gen_event:notify(change_notif, {update, Sub, PrevSub}),
        {true, lists:keyreplace(SubId, #element.id, Subs, Sub)}
    end, State).

delete_sub(SchemaId, SubId, State, Modifier) ->
    Modifier(SchemaId, fun(Subs) ->
        gen_event:notify(change_notif,
            {delete, lists:keyfind(SubId, #element.id, Subs)}),
        {true, lists:keydelete(SubId, #element.id, Subs)}
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
    modify_schema(SchemaId, fun(#schema{elements = Elements} = Schema) ->
        {Ret, NewElements} = Fun(Elements),
        {Ret, Schema#schema{elements = NewElements}}
    end, State).

modify_schema_connections(SchemaId, Fun, State) ->
    modify_schema(SchemaId, fun(#schema{connections = Connections} = Schema) ->
        {Ret, NewConnections} = Fun(Connections),
        {Ret, Schema#schema{connections = NewConnections}}
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

schema_test() ->
    {ok, _PId} = start_link(),
    [] = get_schemas(),
    Schema = #schema{name = "toto"},
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
    {ok, _PId} = gen_event:start_link({local, change_notif}),
    {ok, _PId2} = start_link(),
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
    false = get_element(SchemaId, ElementId),
    stop(),
    gen_event:stop(change_notif).

connection_test() ->
    {ok, _PId} = gen_event:start_link({local, change_notif}),
    {ok, _PId2} = start_link(),
    Schema = #schema{name = "toto"},
    SchemaId = create_schema(Schema),
    Connection = #connection{source_id = 1, source_output = 1, target_id = 1,
        target_input = 1},

    ConnectionId = create_connection(SchemaId, Connection),
    Connection2 = Connection#connection{id = ConnectionId},
    Schema2 = Schema#schema{
        id = SchemaId,
        connections = [Connection2]
    },
    Schema2 = get_schema(SchemaId),

    Connection3 = Connection2#connection{target_input = 2},
    true = update_connection(SchemaId, ConnectionId, Connection3),
    Schema3 = Schema2#schema{connections = [Connection3]},
    Schema3 = get_schema(SchemaId),
    Connection3 = get_connection(SchemaId, ConnectionId),

    delete_connection(SchemaId, ConnectionId),
    Schema4 = Schema3#schema{connections = []},
    Schema4 = get_schema(SchemaId),
    false = get_connection(SchemaId, ConnectionId),
    stop(),
    gen_event:stop(change_notif).
