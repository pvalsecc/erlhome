%%%-------------------------------------------------------------------
%%% @author patrick
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%% Structure of the subscription tree:
%%%   db        %notifs for DB modifications
%%%       create
%%%           {element|connection}
%%%               {SchemaId}
%%%                   {SubId} -> #element{} | #connection{}
%%%       delete/... (same as create)
%%%       update
%%%           element/{SchemaId}/{SubId} -> {New :: #element{}, Old ::#element{}}
%%%           connection/{SchemaId}/{SubId} -> {New :: #connection{}, Old ::#connection{}}
%%%   status    %element's state (mostly for the websocket)
%%%       connection/{SchemaId}/{SubId} -> boolean()
%%%       switch/{SchemaId}/{SubId} -> boolean()
%%%       relay/{SchemaId}/{SubId} -> boolean()
%%%       timer/{SchemaId}/{SubId} -> boolean()
%%%       desc/{SchemaId}/{SubId} -> binary()
%%%   mqtt      %stuff to/from mqtt
%%%       get/{DeviceId}/{InstanceId}/{Class}/... -> any()
%%%       set/{DeviceId}/{InstanceId}/{Class}/... -> any()
%%%       control/{Command} -> any()
%%% @end
%%% Created : 02. May 2015 09:50
%%%-------------------------------------------------------------------
-module(ehome_dispatcher).
-author("patrick").

-behaviour(gen_server).

%% API
-export([start_link/0, subscribe/3, publish/3, stop/0, unsubscribe/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

%% Tests only
-export([sync/0]).


-define(SERVER, ?MODULE).

-type node_id() :: any() | any | all.

-type path() :: [node_id()].

-type listener() :: fun((Topic :: [node_id()], Value :: any()) -> ok).

-record(node, {
    listeners = [] :: [{SubscribeId :: any(), listener()}],
    childs = #{} :: #{node_id() => node()},
    value = undefined
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

-spec(subscribe(Path :: path(), SubscribeId :: any(),
                Listener :: listener()) -> ok).
subscribe(Path, SubscribeId, Listener) ->
    gen_server:cast(?SERVER, {subscribe, Path, SubscribeId, Listener}).

-spec(unsubscribe(SubscribeId :: any()) -> ok).
unsubscribe(SubscribeId) ->
    gen_server:cast(?SERVER, {unsubscribe, SubscribeId}).

-spec(publish(Path :: path(), Value :: any(), Store :: boolean()) -> ok).
publish(Path, Value, Store) ->
    gen_server:cast(?SERVER, {publish, Path, Value, Store}).

%% @doc
%% Make sure the dispatcher has processed every events by sending a synchronous
%% message
%% @end
sync() ->
    gen_server:call(?SERVER, sync).

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
    {ok, State :: #node{}} | {ok, State :: #node{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #node{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #node{}) ->
    {reply, Reply :: term(), NewState :: #node{}} |
    {reply, Reply :: term(), NewState :: #node{}, timeout() | hibernate} |
    {noreply, NewState :: #node{}} |
    {noreply, NewState :: #node{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #node{}} |
    {stop, Reason :: term(), NewState :: #node{}}).
handle_call(stop, _From, State) ->  %UTs only
    {stop, normal, ok, State};
handle_call(sync, _From, State) ->  %UTs only
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #node{}) ->
    {noreply, NewState :: #node{}} |
    {noreply, NewState :: #node{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #node{}}).
handle_cast({subscribe, Path, SubscribeId, Listener}, State) ->
    notify_previous(Path, Listener, State),
    {noreply, subscribe(Path, SubscribeId, Listener, State)};
handle_cast({unsubscribe, SubscribeId}, State) ->
    {noreply, unsubscribe(SubscribeId, State)};
handle_cast({publish, Path, Value, Store}, State) ->
    lager:debug("publish ~p = ~p", [Path, Value]),
    NewState = publish(Path, Value, Store, State),
    {noreply, NewState};
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
-spec(handle_info(Info :: timeout() | term(), State :: #node{}) ->
    {noreply, NewState :: #node{}} |
    {noreply, NewState :: #node{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #node{}}).
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
    State :: #node{}) -> term()).
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #node{},
    Extra :: term()) ->
    {ok, NewState :: #node{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

subscribe([], SubscribeId, Listener, #node{listeners = Listeners} = Node) ->
    Node#node{listeners = [{SubscribeId, Listener} | Listeners]};
subscribe([CurPath | RestPath], SubscribeId, Listener, #node{childs = Childs} = Node) ->
    Child = maps:get(CurPath, Childs, #node{}),
    NewChilds = maps:put(CurPath,
        subscribe(RestPath, SubscribeId, Listener, Child), Childs),
    Node#node{childs = NewChilds}.

filter_map(Fun, Map) ->
    EmptyMap = #{},
    maps:fold(fun(K, V, Acc) ->
        case Fun(K, V) of
            #node{listeners = [], childs = EmptyMap, value = undefined} ->
                Acc;
            Node ->
                maps:put(K, Node, Acc)
        end
    end, #{}, Map).

unsubscribe(SubscribeId, #node{listeners = Listeners, childs = Childs} = Node) ->
    NewListeners = lists:filter(fun({Id, _}) ->
        Id =/= SubscribeId
    end, Listeners),
    NewChilds = filter_map(fun(_K, V) -> unsubscribe(SubscribeId, V) end, Childs),
    Node#node{listeners = NewListeners, childs = NewChilds}.

publish(Path, Value, Store, Node) ->
    publish(Path, Path, Value, Store, Node).

publish(FullPath, [], Value, Store, #node{listeners = Listeners} = Node) ->
    lists:foreach(fun({_Id, Listener}) ->
        Listener(FullPath, Value)
    end, Listeners),
    maybe_store(Node, Value, Store);
publish(FullPath, [CurPath | RestPath], Value, Store, #node{childs = Childs} = Node) ->
    NewChilds = do_publish(FullPath, CurPath, RestPath, Value, Store, Childs),
    do_publish(FullPath, any, RestPath, Value, false, Childs),
    do_publish(FullPath, all, [], Value, false, Childs),
    Node#node{childs = NewChilds}.

do_publish(FullPath, CurPath, RestPath, Value, Store, Childs) ->
    case maps:get(CurPath, Childs, undefined) of
        undefined ->
            maybe_store(Childs, CurPath, RestPath, Value, Store);
        Child ->
            case publish(FullPath, RestPath, Value, Store, Child) of
                Child -> Childs;
                NewChild -> maps:put(CurPath, NewChild, Childs)
            end
    end.

maybe_store(Childs, _CurPath, _RestPath, _Value, false) ->
    Childs;
maybe_store(Childs, CurPath, [], Value, true) ->
    NewChild = #node{value = Value},
    maps:put(CurPath, NewChild, Childs);
maybe_store(Childs, CurPath, [NextPath | RestPath], Value, true) ->
    NewChild = #node{childs = maybe_store(#{}, NextPath, RestPath, Value, true)},
    maps:put(CurPath, NewChild, Childs).

maybe_store(Node, _Value, false) ->
    Node;
maybe_store(Node, Value, true) ->
    Node#node{value = Value}.

notify_previous(Path, Listener, Node) ->
    notify_previous(Path, [], Listener, Node).

notify_previous([], _PrevPath, _Listener, #node{value = undefined}) ->
    ok;
notify_previous([], PrevPath, Listener, #node{value = Value}) ->
    lager:debug("publish again ~p = ~p", [PrevPath, Value]),
    Listener(lists:reverse(PrevPath), Value);
notify_previous([all], PrevPath, Listener, #node{childs = Childs} = Node) ->
    notify_previous([], PrevPath, Listener, Node),
    lists:foreach(fun({NextPath, NextNode}) ->
        notify_previous([all], [NextPath|PrevPath], Listener, NextNode)
    end, maps:to_list(Childs));
notify_previous([any|Rest], PrevPath, Listener, #node{childs = Childs}) ->
    lists:foreach(fun({NextPath, NextNode}) ->
        notify_previous(Rest, [NextPath|PrevPath], Listener, NextNode)
    end, maps:to_list(Childs));
notify_previous([Cur|Rest], PrevPath, Listener, #node{childs = Childs}) ->
    case maps:get(Cur, Childs, undefined) of
        undefined -> ok;
        Child -> notify_previous(Rest, [Cur|PrevPath], Listener, Child)
    end.

%%%===================================================================
%%% Tests
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

recorder() ->
    recorder([]).

recorder(List) ->
    receive
        {record, Value} ->
            recorder([Value | List]);
        {get, From} ->
            From ! lists:reverse(List),
            recorder([])
    end.

get_recorded(Recorder) ->
    Recorder ! {get, self()},
    receive
        Result -> Result
    end.

subscribe_recorder(SubPath) ->
    Recorder = spawn_link(fun recorder/0),
    ok = subscribe(SubPath, Recorder, fun(Path, Value) ->
        Recorder ! {record, {Path, Value}}
    end),
    Recorder.

stop() ->
    gen_server:call(?SERVER, stop).

nominal_test() ->
    test_utils:dispatcher_env(fun() ->
        Recorder1 = subscribe_recorder([1,2,3]),
        Recorder2 = subscribe_recorder([toto,2,3]),
        Recorder3 = subscribe_recorder([toto,any,3]),
        Recorder4 = subscribe_recorder([toto,all]),

        sync(),
        [] = get_recorded(Recorder1),
        [] = get_recorded(Recorder2),
        [] = get_recorded(Recorder3),
        [] = get_recorded(Recorder4),

        ok = publish([toto, 2, 3], 1, false),
        sync(),
        [] = get_recorded(Recorder1),
        [{[toto, 2, 3], 1}] = get_recorded(Recorder2),
        [{[toto, 2, 3], 1}] = get_recorded(Recorder3),
        [{[toto, 2, 3], 1}] = get_recorded(Recorder4),

        ok = unsubscribe(Recorder4),
        ok = publish([toto, 2, 3], 2, false),
        sync(),
        [] = get_recorded(Recorder1),
        [{[toto, 2, 3], 2}] = get_recorded(Recorder2),
        [{[toto, 2, 3], 2}] = get_recorded(Recorder3),
        [] = get_recorded(Recorder4)
    end).

no_subscriber_test() ->
    test_utils:dispatcher_env(fun() ->
        ok = publish([toto, 2, 3], 1, false)
    end).

store_test() ->
    test_utils:dispatcher_env(fun() ->
        ok = publish([toto, 2, 3], 1, true),
        ok = publish([toto, 4, 3], 1, false),

        Recorder1 = subscribe_recorder([1,2,3]),
        Recorder2 = subscribe_recorder([toto,2,3]),
        Recorder3 = subscribe_recorder([toto,any,3]),
        Recorder4 = subscribe_recorder([toto,all]),
        Recorder5 = subscribe_recorder([toto,2,any]),

        sync(),

        [] = get_recorded(Recorder1),
        [{[toto, 2, 3], 1}] = get_recorded(Recorder2),
        [{[toto, 2, 3], 1}] = get_recorded(Recorder3),
        [{[toto, 2, 3], 1}] = get_recorded(Recorder4),
        [{[toto, 2, 3], 1}] = get_recorded(Recorder5)
    end).
