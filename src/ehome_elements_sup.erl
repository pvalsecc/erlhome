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
-export([start_link/0, handle_event/2, iterate_status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-record(element_mapping, {id :: integer(), pid :: pid()}).

-record(state, {
    elements = [] :: #element_mapping{}
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

-spec(iterate_status(Callback :: fun((Type :: atom(), Id :: integer(), Status :: boolean()) -> ok)) -> ok).
iterate_status(Callback) ->
    gen_server:cast(?MODULE, {iterate_status, Callback}). %TODO: implement

handle_event(Sup, Event) ->
    gen_server:cast(Sup, {handle_event, Event}).

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
    ehome_event_forwarder:register(?MODULE, self()),
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
handle_cast({handle_event, Event}, State) ->
    {noreply, handle_event_impl(Event, State)};
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

get_start_func(#element{id = Id, type = <<"and">>}) ->
    {ehome_binary_logic, and_start_link, [Id]};
get_start_func(#element{id = Id, type = <<"or">>}) ->
    {ehome_binary_logic, or_start_link, [Id]};
get_start_func(#element{id = Id, type = <<"xor">>}) ->
    {ehome_binary_logic, or_start_link, [Id]};
get_start_func(#element{id = Id, type = <<"relay">>}) ->
    {ehome_relay, start_link, [Id]};
get_start_func(#element{id = Id, type = <<"switch">>}) ->
    {ehome_switch, start_link, [Id]}.

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

handle_event_impl({create, #element{id = Id} = Element}, State) ->
    {Module, Fun, Args} = get_start_func(Element),
    {ok, Pid} = apply(Module, Fun, Args),
    add_id(Id, Pid, State);

handle_event_impl({delete, #element{id = Id}}, State) ->
    Pid = pid_from_id(Id, State),
    ehome_element:stop(Pid),
    remove_id(Id, State);

handle_event_impl(
        {create, #connection{source_id = SourceId, source_output = SourceOutput,
            target_id = TargetId, target_input = TargetInput, id = Id}},
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

handle_event_impl(
        {delete, #connection{source_id = SourceId, source_output = SourceOutput, id = Id}},
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

handle_event_impl(
        {update, #connection{} = NewConnection, #connection{} = OldConnection},
        State) ->
    NewState = handle_event_impl({delete, OldConnection}, State),
    handle_event_impl({create, NewConnection}, NewState);

handle_event_impl(_Event, State) ->
    State.
