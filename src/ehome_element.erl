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

-behaviour(gen_server).

-callback init(Args :: list()) ->
    State :: any().

-callback new_inputs(Inputs :: list(boolean()),
        OldOutputs :: list(boolean()),
        State :: any()) ->
    NewInner :: any() |
    {NewOutputs :: list(boolean), NewInner :: any()}.

%% API
-export([start_link/5, set_input/3, new_outputs/3, connect/4, get_outputs/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {
    name :: string(),
    implementation :: atom(),
    input_values :: list(boolean()),
    output_values :: list(boolean()),
    output_connections :: list(list({pid(), integer()})),
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
-spec(start_link(Name :: term(), Module :: atom(),
        NbInputs :: integer(), NbOutputs :: integer(), Params :: list()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name, Module, NbInputs, NbOutputs, Params) ->
    gen_server:start_link(?MODULE, [Name, Module, NbInputs, NbOutputs, Params],
        []).

-spec(set_input(Gate :: pid(), Index :: pos_integer(), Value :: boolean()) -> ok).
set_input(Gate, Index, Value) ->
    gen_server:cast(Gate, {set_input, Index, Value}).

new_outputs(Gate, NewOutputs, NewInner) ->
    gen_server:cast(Gate, {new_outputs, NewOutputs, NewInner}).

-spec(connect(Gate :: pid(), Output :: pos_integer(), Destination :: pid(),
        Input :: pos_integer()) -> ok).
connect(Gate, Output, Destination, Input) ->
    gen_server:cast(Gate, {connect, Output, Destination, Input}).

get_outputs(Gate) ->
    gen_server:call(Gate, {get_outputs}).

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
init([Name, Module, NbInputs, NbOutputs, Params]) ->
    {ok, #state{name = Name,
        implementation = Module,
        input_values = false_list(NbInputs),
        output_values = false_list(NbOutputs),
        output_connections = create_list(NbOutputs, []),
        inner_state = Module:init(Params)}}.

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
handle_call({get_outputs}, _From, #state{output_values = Outputs} = State) ->
    {reply, Outputs, State}.

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
handle_cast({set_input, Index, Value},
        #state{implementation = Impl, input_values = Inputs,
            output_values = Outputs,
            inner_state = Inner} = State) ->
    NewInputs = replace_list(Inputs, Index, Value),
    case Impl:new_inputs(NewInputs, Outputs, Inner) of
        {NewOutputs, NewInner} ->
            handle_new_outputs(NewOutputs, NewInner,
                State#state{input_values = NewInputs});
        NewInner ->
            {noreply, State#state{input_values = NewInputs, inner_state = NewInner}}
    end;

handle_cast({new_outputs, NewOutputs, NewInner}, State) ->
    handle_new_outputs(NewOutputs, NewInner, State);

handle_cast({connect, Output, Destination, Input},
        #state{output_connections = Connections} = State) ->
    Cur = lists:nth(Output, Connections),
    NewConnections =
        replace_list(Connections, Output, [{Destination, Input} | Cur]),
    {noreply, State#state{output_connections = NewConnections}}.

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
        #state{name = Name, output_values = Outputs, output_connections = Connections} =
            State) ->
    io:format("~s: ~w~n", [Name, NewOutputs]),
    notify(Outputs, NewOutputs, Connections),
    {noreply, State#state{output_values = NewOutputs, inner_state = NewInner}}.

false_list(N) -> create_list(N, false).

create_list(0, _Value) ->
    [];
create_list(N, Value) ->
    [Value | create_list(N - 1, Value)].

replace_list([_ | Rest], 1, Value) ->
    [Value | Rest];
replace_list([Head | Rest], Index, Value) ->
    [Head | replace_list(Rest, Index - 1, Value)].

notify([], [], []) ->
    undefined;
notify([H | ROld], [H | RNew], [_ | RCon]) ->
    %Output not changed
    notify(ROld, RNew, RCon);
notify([_HOld | ROld], [HNew | RNew], [HCon | RCon]) ->
    notify_all(HNew, HCon),
    notify(ROld, RNew, RCon).

notify_all(_Value, []) ->
    undefined;
notify_all(Value, [{Pid, Input} | Rest]) ->
    set_input(Pid, Input, Value),
    notify_all(Value, Rest).

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
