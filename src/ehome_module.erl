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

-export([init/1, iterate_status/3, new_inputs/3, control/3]).

-behavior(ehome_element).

-include("ehome_types.hrl").

-record(state, {schema_id, id, status = false, mqtt_path}).

start_link(SchemaId, Id, Config) ->
    ehome_element:start_link(SchemaId, Id, ehome_module, 2, 1,
        {SchemaId, Id, Config}).

init({SchemaId, Id, Config}) ->
    MqttPath = subscribe(ehome_utils:parse_path(maps:get(<<"mqtt_path">>,
        Config, undefined))),
    Status = get_value(MqttPath),
    State = #state{schema_id = SchemaId, id = Id, mqtt_path = MqttPath,
        status = Status},
    {[Status], notif_web(State)}.

new_inputs([true, _], [false, _], State) ->
    force(State, true);
new_inputs([_, true], [_, false], State) ->
    force(State, false);
new_inputs(_Inputs, _OldInputs, State) ->
    State.

iterate_status(Callback, Acc, #state{id = Id, status = Status}) ->
    Callback(#status{type = switch, id = Id, value = Status}, Acc).

control(config, #{<<"mqtt_path">> := MqttPath}, State) ->
    case subscribe(ehome_utils:parse_path(MqttPath)) of
        undefined -> false;
        NewMqttPath ->
            Status = get_value(NewMqttPath),
            NewState = State#state{mqtt_path = NewMqttPath, status = Status},
            {new_outputs, [Status], notif_web(NewState)}
    end;
control(switch, Value, #state{status = Value} = State) ->
    State; %no change
control(switch, Value, State) when is_boolean(Value) ->
    %comes from MQTT
    lager:info("Update from MQTT: ~p", [Value]),
    {new_outputs, [Value], notif_web(State#state{status = Value})};
control(<<"toggle">>, true, #state{status = Value} = State) ->
    force(State, not Value);
control(Type, Message, _Inner) ->
    lager:error("ehome_module: un-supported message ~p/~p", [Type, Message]),
    false.

subscribe(undefined) ->
    undefined;
subscribe(Path) ->
    Self = self(),
    ok = ehome_dispatcher:unsubscribe(Self),
    ok = ehome_dispatcher:subscribe([mqtt, get | Path], Self, fun(_Topic, Value) ->
        ehome_element:control(Self, switch, Value)
    end),
    Path.

force(#state{status = Value} = State, Value) ->
    State; %no value change
force(State, Value) ->
    notif_mqtt(State, Value).

notif_web(#state{schema_id = SchemaId, id = Id, status = Status} = State) ->
    ehome_dispatcher:publish([status, switch, SchemaId, Id], Status),
    State.

notif_mqtt(#state{mqtt_path = undefined} = State, _Status) ->
    State;
notif_mqtt(#state{mqtt_path = Path} = State, Status) ->
    ehome_dispatcher:publish([mqtt, set | Path], Status),
    State.

get_value(undefined) ->
    false;
get_value(Path) ->
    ehome_utils:maybe(ehome_mqtt_tree:get_value(Path), false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

nominal_test() ->
    test_utils:mqtt_tree_env(fun() ->
        dispatcher_recorder:test_env([mqtt, set, all], fun() ->
            {ok, Module} = start_link(1, 2,
                #{<<"mqtt_path">> => <<"[2,1,switch_binary,\"level\"]">>}),
            test_utils:wait_queue_empty(Module),
            [false] = ehome_element:get_outputs(Module),

            ehome_element:set_input(Module, 1, true),
            test_utils:wait_queue_empty(Module),
            [{[mqtt,set,2,1,switch_binary,"level"], true}] =
                dispatcher_recorder:get_events(),
            [false] = ehome_element:get_outputs(Module),

            ehome_dispatcher:publish([mqtt, get, 2, 1, switch_binary,
                "level"], true),
            ehome_dispatcher:sync(),
            test_utils:wait_queue_empty(Module),
            [true] = ehome_element:get_outputs(Module),

            ehome_element:set_input(Module, 1, false),
            test_utils:wait_queue_empty(Module),
            [] = dispatcher_recorder:get_events(),
            [true] = ehome_element:get_outputs(Module),

            ehome_element:set_input(Module, 2, true),
            test_utils:wait_queue_empty(Module),
            [{[mqtt,set,2,1,switch_binary,"level"], false}] =
                dispatcher_recorder:get_events(),
            [true] = ehome_element:get_outputs(Module),

            ehome_dispatcher:publish([mqtt, get, 2, 1, switch_binary,
                "level"], false),
            ehome_dispatcher:sync(),
            test_utils:wait_queue_empty(Module),
            [false] = ehome_element:get_outputs(Module),

            ehome_element:set_input(Module, 2, false),
            test_utils:wait_queue_empty(Module),
            [] = dispatcher_recorder:get_events(),
            [false] = ehome_element:get_outputs(Module)
        end)
    end).
