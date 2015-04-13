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
-export([start_link/0, get_schemas/0, get_schema/1, create_schema/1,
    update_schema/2, delete_schema/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-include("ehome_types.hrl").

-record(state, {schemas = [] :: [#schema{}], next_id :: integer()}).

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


-spec(get_schemas() -> [#schema{}]).
get_schemas() ->
    gen_server:call(?SERVER, {get_schemas}).

-spec(get_schema(ID :: integer()) -> #schema{}).
get_schema(ID) ->
    gen_server:call(?SERVER, {get_schema, ID}).

-spec(create_schema(#schema{}) -> ID :: integer()).
create_schema(#schema{id = undefined} = Schema) ->
    gen_server:call(?SERVER, {create_schema, Schema}).

-spec(update_schema(ID :: integer(), #schema{}) -> boolean()).
update_schema(ID, #schema{id = undefined} = Schema) ->
    gen_server:call(?SERVER, {update_schema, ID, Schema});
update_schema(ID, #schema{id = ID} = Schema) ->
    gen_server:call(?SERVER, {update_schema, ID, Schema}).

-spec(delete_schema(integer()) -> true|false).
delete_schema(ID) ->
    gen_server:call(?SERVER, {delete_schema, ID}).


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
    {ok, #state{next_id = 1}}.

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
handle_call({get_schema, ID}, _From, #state{schemas = Schemas} = State) ->
    {reply, get_schema(ID, Schemas), State};
handle_call({create_schema, Schema}, _From, State) ->
    {ID, NewState} = add_schema(Schema, State),
    {reply, ID, NewState};
handle_call({update_schema, ID, Schema}, _From, State) ->
    {Result, NewState} = update_schema(ID, Schema, State),
    {reply, Result, NewState};
handle_call({delete_schema, ID}, _From, State) ->
    {Result, NewState} = delete_schema(ID, State),
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
handle_cast(_Request, State) ->
    {noreply, State}.

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

add_schema(Schema, #state{next_id = ID, schemas = Schemas} = State) ->
    NewSchema = Schema#schema{id = ID},
    {ID, State#state{next_id = ID + 1, schemas = [NewSchema | Schemas]}}.

get_schema(ID, Schemas) ->
    lists:keyfind(ID, #schema.id, Schemas).

update_schema(ID, Schema, #state{schemas = Schemas} = State) ->
    NewSchema = Schema#schema{id = ID},
    NewSchemas = lists:keyreplace(ID, #schema.id, Schemas, NewSchema),
    {true, State#state{schemas=NewSchemas}}.

delete_schema(ID, #state{schemas = Schemas} = State) ->
    case lists:keydelete(ID, #schema.id, Schemas) of
        Schemas -> {false, State};
        NewSchemas -> {true, State#state{schemas = NewSchemas}}
    end.



%%%===================================================================
%%% Tests
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

add_schema_test() ->
    NextId = 12,
    State = #state{next_id = NextId},
    Schema = #schema{name = "toto"},
    ExpectedSchema = Schema#schema{id = NextId},
    ExpectedState = State#state{next_id = NextId + 1,
                                schemas = [ExpectedSchema]},
    {NextId, ExpectedState} = add_schema(Schema, State).

schema_test() ->
    {ok, _PID} = start_link(),
    [] = get_schemas(),
    Schema = #schema{name="toto"},
    1 = ID = create_schema(Schema),
    NewSchema = Schema#schema{id = ID},
    [NewSchema] = get_schemas(),
    NewSchema = get_schema(ID),
    Renamed = NewSchema#schema{name = "titi"},
    true = update_schema(ID, Renamed),
    Renamed = get_schema(ID),
    false = delete_schema(ID + 1),
    [Renamed] = get_schemas(),
    true = delete_schema(ID),
    [] = get_schemas().
