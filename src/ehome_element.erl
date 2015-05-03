%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, Patrick Valsecchi
%%% @doc
%%% Behaviour for a logic gate
%%% @end
%%% Created : 10. Apr 2015 2:06 PM
%%%-------------------------------------------------------------------
-module(ehome_element).
-author("pvalsecc").

-include("ehome_types.hrl").

-behaviour(gen_server).

-callback init(Args :: any()) -> {Outputs :: [boolean()], State :: any()}.

-callback new_inputs(Inputs :: [boolean()], OldOutputs :: list(boolean()),
                     State :: any()) ->
    NewInner :: any() |
    {new_outputs, NewOutputs :: [boolean()], NewInner :: any()}.

-callback iterate_status(Callback :: status_callback(), Acc :: any(),
        Inner :: any()) -> any().

-callback control(Type :: binary(), Message :: any(), Inner :: any()) ->
    {new_outputs, NewOutputs :: [boolean()], NewInner :: any()} |
    any()  | false.

%% API
-export([start_link/6, set_input/3, new_outputs/2, connect/5, disconnect/3,
    get_inputs/1, get_outputs/1, stop/1, iterate_status/3, control/3]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {
    schema_id :: integer(),
    id :: integer(),
    implementation :: atom(),
    input_values :: list(boolean()),
    output_values :: list(boolean()),
    output_connections :: list(list({Id :: integer(), Target :: pid(), Input :: integer()})),
    inner_state :: any()
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
-spec(start_link(SchemaId :: integer(), Id :: integer(), Module :: atom(),
        NbInputs :: integer(), NbOutputs :: integer(), Params :: any()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(SchemaId, Id, Module, NbInputs, NbOutputs, Params) ->
    gen_server:start_link(?MODULE,
        [SchemaId, Id, Module, NbInputs, NbOutputs, Params], []).

-spec(set_input(Gate :: pid(), Index :: pos_integer(), Value :: boolean()) -> ok).
set_input(Gate, Index, Value) ->
    gen_server:cast(Gate, {set_input, Index, Value}).

-spec(new_outputs(Gate :: pid(), NewOutputs :: [boolean()]) -> ok).
new_outputs(Gate, NewOutputs) ->
    gen_server:cast(Gate, {new_outputs, NewOutputs}).

-spec(connect(Gate :: pid(), Output :: pos_integer(), Destination :: pid(),
        Input :: pos_integer(), Id :: integer()) -> ok).
connect(Gate, Output, Destination, Input, Id) ->
    gen_server:cast(Gate, {connect, Output, Destination, Input, Id}).

-spec(disconnect(Gate :: pid(), Output :: pos_integer(), Id :: integer()) -> ok).
disconnect(Gate, Output, Id) ->
    gen_server:cast(Gate, {disconnect, Output, Id}).

get_outputs(Gate) ->
    gen_server:call(Gate, get_outputs).

get_inputs(Gate) ->
    gen_server:call(Gate, get_inputs).

stop(Gate) ->
    gen_server:cast(Gate, stop).

-spec(iterate_status(Gate :: pid(), Callback :: status_callback(),
                     Acc :: any) -> any()).
iterate_status(Gate, Callback, Acc) ->
    gen_server:call(Gate, {iterate_status, Callback, Acc}).

-spec(control(Gate :: pid(), Type :: binary(), Message :: any()) -> true|false).
control(Gate, Type, Message) ->
    gen_server:call(Gate, {control, Type, Message}).

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
init([SchemaId, Id, Module, NbInputs, NbOutputs, Params]) ->
    {Outputs, InnerState} = Module:init(Params),
    NbOutputs = length(Outputs),
    {ok, #state{
        schema_id = SchemaId,
        id = Id,
        implementation = Module,
        input_values = false_list(NbInputs),
        output_values = Outputs,
        output_connections = create_list(NbOutputs, []),
        inner_state = InnerState
    }}.

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
handle_call(get_outputs, _From, #state{output_values = Outputs} = State) ->
    {reply, Outputs, State};
handle_call(get_inputs, _From, #state{input_values = Inputs} = State) ->
    {reply, Inputs, State};
handle_call({iterate_status, Callback, Acc}, _From,
        #state{implementation = Impl, inner_state = Inner,
            output_values = Values,
            output_connections = Connections} = State) ->
    Acc1 = Impl:iterate_status(Callback, Acc, Inner),
    Acc2 = iterate_status_outputs(Callback, Acc1, Values, Connections),
    {reply, Acc2, State};
handle_call({control, Type, Message}, _From,
        #state{implementation = Impl, inner_state = Inner} = State) ->
    case Impl:control(Type, Message, Inner) of
        {new_outputs, NewOutputs, NewInner} ->
            {noreply, NewState} =
                handle_new_outputs(NewOutputs, NewInner, State),
            {reply, true, NewState};
        false ->
            {reply, false, State};
        NewInner ->
            {reply, true, State#state{inner_state = NewInner}}
    end.


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
handle_cast({set_input, Index, Value}, #state{input_values = Inputs} = State) ->
    NewInputs = replace_list(Inputs, Index, Value),
    handle_inputs(State#state{input_values = NewInputs});

handle_cast({new_outputs, NewOutputs}, #state{inner_state = Inner} = State) ->
    handle_new_outputs(NewOutputs, Inner, State);

handle_cast({connect, Output, Destination, Input, Id},
        #state{output_connections = Connections,
               output_values = Values} = State) ->
    Cur = lists:nth(Output, Connections),
    NewConnections =
        replace_list(Connections, Output, [{Id, Destination, Input} | Cur]),
    NewState = State#state{output_connections = NewConnections},
    Value = lists:nth(Output, Values),
    notify_all(Value, [{Id, Destination, Input}], NewState),
    {noreply, NewState};

handle_cast({disconnect, Output, Id},
        #state{output_connections = Connections} = State) ->
    Cur = lists:nth(Output, Connections),
    NewConnections =
        replace_list(Connections, Output,
            lists:keydelete(Id, 1,  Cur)),
    {noreply, State#state{output_connections = NewConnections}};

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

handle_new_outputs(NewOutputs, NewInner,
        #state{id = Id, output_values = Outputs, output_connections = Connections} =
            State) ->
    io:format("~p: output=~w~n", [Id, NewOutputs]),
    notify(Outputs, NewOutputs, Connections, State),
    {noreply, State#state{output_values = NewOutputs, inner_state = NewInner}}.

handle_inputs(#state{implementation = Impl, input_values = Inputs,
    output_values = Outputs, inner_state = Inner} = State) ->
    case Impl:new_inputs(Inputs, Outputs, Inner) of
        {new_outputs, NewOutputs, NewInner} ->
            handle_new_outputs(NewOutputs, NewInner, State);
        NewInner ->
            {noreply, State#state{inner_state = NewInner}}
    end.

false_list(N) -> create_list(N, false).

create_list(0, _Value) ->
    [];
create_list(N, Value) ->
    [Value | create_list(N - 1, Value)].

replace_list([_ | Rest], 1, Value) ->
    [Value | Rest];
replace_list([Head | Rest], Index, Value) ->
    [Head | replace_list(Rest, Index - 1, Value)].

remove_first_match(_ToRemove, []) ->
    [];
remove_first_match(ToRemove, [ToRemove|Rest]) ->
    Rest;
remove_first_match(ToRemove, [Cur|Rest]) ->
    [Cur|remove_first_match(ToRemove, Rest)].

notify([], [], [], _State) ->
    undefined;
notify([H | ROld], [H | RNew], [_ | RCon], State) ->
    %Output not changed
    notify(ROld, RNew, RCon, State);
notify([_HOld | ROld], [HNew | RNew], [HCon | RCon], State) ->
    notify_all(HNew, HCon, State),
    notify(ROld, RNew, RCon, State).

notify_all(_Value, [], _State) ->
    undefined;
notify_all(Value, [{Id, Pid, Input} | Rest],
        #state{schema_id = SchemaId} = State) ->
    set_input(Pid, Input, Value),
    ehome_dispatcher:publish([status, connection, SchemaId, Id], Value),
    notify_all(Value, Rest, State).

iterate_status_outputs(_Callback, Acc, [], []) ->
    Acc;
iterate_status_outputs(Callback, Acc, [Value|VRest], [Connections|CRest]) ->
    Acc1 = iterate_status_output(Callback, Acc, Value, Connections),
    iterate_status_outputs(Callback, Acc1, VRest, CRest).

iterate_status_output(_Callback, Acc, _Value, []) ->
    Acc;
iterate_status_output(Callback, Acc, Value, [{Id, _, _}|Rest]) ->
    Acc1 = Callback(connection_notif(Id, Value), Acc),
    iterate_status_output(Callback, Acc1, Value, Rest).

connection_notif(Id, Value) ->
    #status{type = connection, id = Id, value = Value}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTs

-include_lib("eunit/include/eunit.hrl").

false_list_test() ->
    [] = false_list(0),
    [false] = false_list(1),
    [false, false, false] = false_list(3).

replace_list_test() ->
    List = [1, 2, 3],
    [x, 2, 3] = replace_list(List, 1, x),
    [1, x, 3] = replace_list(List, 2, x),
    [1, 2, x] = replace_list(List, 3, x).

remove_first_match_test() ->
    List = [1, 2, 3, 2],
    [1, 3, 2] = remove_first_match(2, List),
    List = remove_first_match(12, List).
