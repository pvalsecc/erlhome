%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Apr 2015 10:31 AM
%%%-------------------------------------------------------------------
-module(ehome_elements_sup).
-author("pvalsecc").

-behaviour(gen_server).

%% API
-export([start_link/0, control/3, stop/1, handle_event/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-record(element_mapping, {id :: integer(), pid :: pid()}).

-record(state, {
    elements = [] :: [#element_mapping{}]
}).

-include("ehome_types.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec(control(Id :: integer(), Type :: binary(), Message :: any()) ->
    true|false).
control(Id, Type, Message) ->
    gen_server:call(?MODULE, {control, Id, Type, Message}).

handle_event(Topic, Event) ->
    gen_server:cast(?MODULE, {handle_event, Topic, Event}).

stop(Sup) ->
    gen_server:call(Sup, stop).

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
    process_flag(trap_exit, true),
    Self = self(),
    ehome_dispatcher:subscribe([db, all], Self, fun(Topic, Event) ->
        gen_server:cast(Self, {handle_event, Topic, Event})
    end),
    {ok, #state{}}.

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
handle_call({control, Id, Type, Message}, _From, State) ->
    {reply, control(Id, Type, Message, State), State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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
handle_cast({handle_event, Topic, Event}, State) ->
    {noreply, handle_event(Topic, Event, State)};
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
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};  %normal stop of an element
handle_info({'EXIT', _Pid, Reason}, State) ->
    %TODO: restart the element
    io:format("element EXIT: ~p~n", [Reason]),
    {noreply, State};
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
    ehome_dispatcher:unsubscribe(self()),
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

get_start_func(SchemaId, #element{id = Id, type = <<"and">>}) ->
    {ehome_21_gate, and_start_link, [SchemaId, Id]};
get_start_func(SchemaId, #element{id = Id, type = <<"or">>}) ->
    {ehome_21_gate, or_start_link, [SchemaId, Id]};
get_start_func(SchemaId, #element{id = Id, type = <<"xor">>}) ->
    {ehome_21_gate, xor_start_link, [SchemaId, Id]};
get_start_func(SchemaId, #element{id = Id, type = <<"not">>}) ->
    {ehome_11_gate, not_start_link, [SchemaId, Id]};
get_start_func(SchemaId, #element{id = Id, type = <<"relay">>}) ->
    {ehome_relay, start_link, [SchemaId, Id]};
get_start_func(SchemaId, #element{id = Id, type = <<"switch">>}) ->
    {ehome_switch, start_link, [SchemaId, Id]};
get_start_func(SchemaId, #element{id = Id, type = <<"up_edge">>}) ->
    {ehome_edge_gate, up_start_link, [SchemaId, Id]};
get_start_func(SchemaId, #element{id = Id, type = <<"down_edge">>}) ->
    {ehome_edge_gate, down_start_link, [SchemaId, Id]};
get_start_func(SchemaId, #element{id = Id, type = <<"both_edge">>}) ->
    {ehome_edge_gate, both_start_link, [SchemaId, Id]};
get_start_func(SchemaId, #element{id = Id, type = <<"d_flipflop">>}) ->
    {ehome_d_flipflop, start_link, [SchemaId, Id]};
get_start_func(SchemaId, #element{id = Id, type = <<"force_off">>}) ->
    {ehome_force_off, start_link, [SchemaId, Id]};
get_start_func(SchemaId, #element{id = Id, type = <<"timer">>,
    config = Config}) ->
    {ehome_timer_gate, start_link, [SchemaId, Id, Config]};
get_start_func(SchemaId, #element{id = Id, type = <<"module">>,
    config = Config}) ->
    {ehome_module, start_link, [SchemaId, Id, Config]}.

pid_from_id(Id, #state{elements = Elements}) ->
    case lists:keyfind(Id, #element_mapping.id, Elements) of
        false ->
            io:format("Unknown element: ~p~n", [Id]),
            false;
        #element_mapping{pid = Pid} -> Pid
    end.

add_id(Id, Pid, #state{elements = Elements} = State) ->
    State#state{elements = [#element_mapping{id = Id, pid = Pid} | Elements]}.

remove_id(Id, #state{elements = Elements} = State) ->
    State#state{elements = lists:keydelete(Id, #element_mapping.id, Elements)}.

handle_event([db, create, element, SchemaId, Id], #element{} = Element, State) ->
    {Module, Fun, Args} = get_start_func(SchemaId, Element),
    {ok, Pid} = apply(Module, Fun, Args),
    add_id(Id, Pid, State);

handle_event([db, update, element, _SchemaId, _Id],
    {#element{config = Config}, #element{config = Config}}, State) ->
    %No config change, nothing to do
    State;

handle_event([db, update, element, SchemaId, Id],
    {#element{config = NewConfig}, OldElement}, State) ->
    Pid = pid_from_id(Id, State),
    case ehome_element:control(Pid, config, NewConfig) of
        false ->
            %TODO: send error to web
            ehome_db:update_element(SchemaId, Id, OldElement);
        _ -> ok
    end,
    State;

handle_event([db, delete, element, _SchemaId, Id], #element{}, State) ->
    Pid = pid_from_id(Id, State),
    ehome_element:stop(Pid),
    remove_id(Id, State);

handle_event([db, create, connection, _SchemaId, Id],
        #connection{source_id = SourceId, source_output = SourceOutput,
                    target_id = TargetId, target_input = TargetInput},
        State) ->
    SourcePid = pid_from_id(SourceId, State),
    TargetPid = pid_from_id(TargetId, State),
    if
        (SourcePid =/= false) and (TargetPid =/= false) ->
            ehome_element:connect(SourcePid, SourceOutput, TargetPid,
                TargetInput, Id);
        true ->
            io:format("Create connection: Cannot find source or target element"),
            false
    end,
    State;

handle_event([db, delete, connection, _SchemaId, Id],
        #connection{source_id = SourceId, source_output = SourceOutput},
        State) ->
    SourcePid = pid_from_id(SourceId, State),
    if
        (SourcePid =/= false) ->
            ehome_element:disconnect(SourcePid, SourceOutput, Id);
        true ->
            io:format("Delete connection: Cannot find source or target element"),
            false
    end,
    State;

handle_event([db, update, connection, SchemaId, Id],
        {#connection{} = NewConnection, #connection{} = OldConnection},
        State) ->
    NewState = handle_event([db, delete, connection, SchemaId, Id],
        OldConnection, State),
    handle_event([db, create, connection, SchemaId, Id],
        NewConnection, NewState);

handle_event(Path, _Event, State) ->
    io:format("ehome_elements_sup: unknown event: ~p~n", [Path]),
    State.

control(Id, Type, Message, State) ->
    case pid_from_id(Id, State) of
        false -> false;
        Pid -> ehome_element:control(Pid, Type, Message)
    end.
