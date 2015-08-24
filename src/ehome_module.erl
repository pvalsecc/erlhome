%%%-------------------------------------------------------------------
%%% @author patrick
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. juin 2015 09:16
%%%-------------------------------------------------------------------
-module(ehome_module).
-author("patrick").

%% API
-export([start_link/3]).

-export([init/1, new_inputs/3, control/3]).

-behavior(ehome_element).

-include("ehome_types.hrl").

-define(RETRY_INTERVAL, 2000).

-record(state, {
    schema_id :: integer(),
    id :: integer(),
    status = false :: boolean(),
    mqtt_path :: list(),
    timeout :: pos_integer(),
    timer_ref = undefined :: timer:tref() | undefined
}).

start_link(SchemaId, Id, Config) ->
    ehome_element:start_link(SchemaId, Id, ehome_module, 2, 1,
        {SchemaId, Id, Config}).

init({SchemaId, Id, Config}) ->
    MqttPath = ehome_utils:parse_path(maps:get(<<"mqtt_path">>,
        Config, undefined)),
    State = #state{schema_id = SchemaId, id = Id, mqtt_path = MqttPath,
        timeout = application:get_env(erlhome, module_retry_interval,
            ?RETRY_INTERVAL)},
    subscribe(MqttPath, State),
    {[false], notif_web(State)}.

new_inputs([true, _], [false, _], State) ->
    force(State, true);
new_inputs([_, true], [_, false], State) ->
    force(State, false);
new_inputs(_Inputs, _OldInputs, State) ->
    State.

control(config, #{<<"mqtt_path">> := MqttPath}, State) ->
    case subscribe(ehome_utils:parse_path(MqttPath), State) of
        undefined -> false;
        NewMqttPath ->
            State#state{mqtt_path = NewMqttPath}
    end;
control(switch, Value, #state{status = Value} = State) ->
    State; %no change
control(switch, Value, State) when is_boolean(Value) ->
    %comes from MQTT
    lager:info("Update from MQTT: ~p", [Value]),
    State2 = cancel_timer(State),
    {new_outputs, [Value], notif_web(State2#state{status = Value})};
control(<<"toggle">>, true, #state{status = Value} = State) ->
    force(State, not Value);
control(<<"switch">>, Value, State) ->
    lager:warning("Timeout setting the module's value to ~p", [Value]),
    force(State, Value);
control(Type, Message, _Inner) ->
    lager:error("ehome_module: un-supported message ~p/~p", [Type, Message]),
    false.

subscribe(undefined, State) ->
    notif_desc(<<"">>, State),
    undefined;
subscribe(Path, State) ->
    Self = self(),
    notif_desc(get_desc(Path), State),
    ok = ehome_dispatcher:unsubscribe(Self),
    ok = ehome_dispatcher:subscribe([mqtt, get | Path], Self, fun(_Topic, Value) ->
        ehome_element:control(Self, switch, Value)
    end),
    Path.

notif_desc(Desc, #state{schema_id = SchemaId, id = Id}) ->
    ehome_dispatcher:publish([status, desc, SchemaId, Id], Desc, true).

get_desc([DeviceId, InstanceId | _]) ->
    case ehome_map_service:get(names, [DeviceId, InstanceId]) of
        undefined ->
            Name = io_lib:format("~p/~p", [DeviceId, InstanceId]),
            list_to_binary(Name);
        Name -> Name
    end.

force(#state{status = Value} = State, Value) ->
    State; %no value change
force(State, Value) ->
    State2 = start_timer(State, Value),
    notif_mqtt(State2, Value).

notif_web(#state{schema_id = SchemaId, id = Id, status = Status} = State) ->
    ehome_dispatcher:publish([status, switch, SchemaId, Id], Status, true),
    State.

notif_mqtt(#state{mqtt_path = undefined} = State, _Status) ->
    State;
notif_mqtt(#state{mqtt_path = Path} = State, Status) ->
    ehome_dispatcher:publish([mqtt, set | Path], Status, false),
    State.

start_timer(#state{timeout = Timeout} = State, Value) ->
    State2 = cancel_timer(State),
    {ok, Timer} = timer:apply_after(Timeout, ehome_element, control,
        [self(), <<"switch">>, Value]),
    State2#state{timer_ref = Timer}.

cancel_timer(#state{timer_ref = undefined} = State) ->
    State;
cancel_timer(#state{timer_ref = Timer} = State) ->
    timer:cancel(Timer),
    State#state{timer_ref = undefined}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

test_env(Test) ->
    test_utils:mqtt_tree_env(fun() ->
        test_utils:names_env(fun() ->
            dispatcher_recorder:test_env([mqtt, set, all], fun() ->
                Test()
            end)
        end)
    end).

nominal_test() ->
    test_env(fun() ->
        {ok, Module} = start_link(1, 2,
            #{<<"mqtt_path">> => <<"[2,1,switch_binary,\"level\"]">>}),
        test_utils:wait_queue_empty(Module),
        [false] = ehome_element:get_outputs(Module),

        ehome_element:set_input(Module, 1, true),
        test_utils:wait_queue_empty(Module),
        [{[mqtt, set, 2, 1, switch_binary, "level"], true}] =
            dispatcher_recorder:get_events(),
        [false] = ehome_element:get_outputs(Module),

        ehome_dispatcher:publish([mqtt, get, 2, 1, switch_binary,
            "level"], true, false),
        ehome_dispatcher:sync(),
        test_utils:wait_queue_empty(Module),
        [true] = ehome_element:get_outputs(Module),

        ehome_element:set_input(Module, 1, false),
        test_utils:wait_queue_empty(Module),
        [] = dispatcher_recorder:get_events(),
        [true] = ehome_element:get_outputs(Module),

        ehome_element:set_input(Module, 2, true),
        test_utils:wait_queue_empty(Module),
        [{[mqtt, set, 2, 1, switch_binary, "level"], false}] =
            dispatcher_recorder:get_events(),
        [true] = ehome_element:get_outputs(Module),

        ehome_dispatcher:publish([mqtt, get, 2, 1, switch_binary,
            "level"], false, false),
        ehome_dispatcher:sync(),
        test_utils:wait_queue_empty(Module),
        [false] = ehome_element:get_outputs(Module),

        ehome_element:set_input(Module, 2, false),
        test_utils:wait_queue_empty(Module),
        [] = dispatcher_recorder:get_events(),
        [false] = ehome_element:get_outputs(Module)
    end).

retry_test() ->
    test_env(fun() ->
        TestTimeout = 50,
        Margin = 10,
        test_utils:app_env_env(module_retry_interval, TestTimeout, fun() ->
            {ok, Module} = start_link(1, 2,
                #{<<"mqtt_path">> => <<"[2,1,switch_binary,\"level\"]">>}),
            test_utils:wait_queue_empty(Module),
            [false] = ehome_element:get_outputs(Module),

            ehome_element:set_input(Module, 1, true),
            test_utils:wait_queue_empty(Module),
            [{[mqtt, set, 2, 1, switch_binary, "level"], true}] =
                dispatcher_recorder:get_events(),

            timer:sleep(TestTimeout + Margin),
            [{[mqtt, set, 2, 1, switch_binary, "level"], true}] =
                dispatcher_recorder:get_events(),

            ehome_dispatcher:publish([mqtt, get, 2, 1, switch_binary,
                "level"], true, false),
            ehome_dispatcher:sync(),
            test_utils:wait_queue_empty(Module),
            [true] = ehome_element:get_outputs(Module),

            timer:sleep(TestTimeout + Margin),
            [] = dispatcher_recorder:get_events()
        end)
    end).
