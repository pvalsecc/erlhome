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
    MqttPath = subscribe(parse_path(maps:get(<<"mqtt_path">>, Config, undefined))),
    State = #state{schema_id = SchemaId, id = Id, mqtt_path = MqttPath},
    {[false], State}.

new_inputs([true, _], [false, _], State) ->
    force(State, true);
new_inputs([_, true], [_, false], State) ->
    force(State, false);
new_inputs(_Inputs, _OldInputs, State) ->
    State.

iterate_status(Callback, Acc, #state{id = Id, status = Status}) ->
    Callback(#status{type = switch, id = Id, value = Status}, Acc).

control(config, #{<<"mqtt_path">> := MqttPath}, State) ->
    case subscribe(parse_path(MqttPath)) of
        undefined -> false;
        NewMqttPath -> State#state{mqtt_path = NewMqttPath}
    end;
control(switch, Value, #state{status = Value} = State) ->
    State; %no change
control(switch, Value, #state{status = Value} = State) when is_boolean(Value) ->
    %comes from MQTT
    lager:info("Update from MQTT: ~p", [Value]),
    {new_outputs, [Value], notif_web(State#state{status = Value})};
control(Type, Message, _Inner) ->
    lager:error("ehome_module: un-supported message ~p/~p",
        [Type, Message]),
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

parse_path(undefined) ->
    undefined;
parse_path(Path) when is_binary(Path) ->
    parse_path(binary_to_list(Path));
parse_path(Path) when is_list(Path) ->
    case erl_scan:string(Path ++ ".") of
        {ok, Tokens, _} -> case erl_parse:parse_term(Tokens) of
                               {ok, Val} when is_list(Val) ->
                                   Val;
                               {ok, Val} ->
                                   lager:error("Not a list: ~p", [Val]),
                                   undefined;
                               {error, Reason} ->
                                   lager:error("Cannot parse <~s>: ~p", [Path, Reason]),
                                   undefined
                           end;
        {error, ErrorInfo, _} ->
            lager:error("Cannot tokenize <~s>: ~p", [Path, ErrorInfo]),
            undefined
    end.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("eunit/include/eunit.hrl").

nominal_test() ->
    ehome_dispatcher:start_link(),
    {ok, Module} = start_link(1, 2,
        #{<<"mqtt_path">> => <<"2/1/switch_binary/data/value">>}),
    test_utils:wait_queue_empty(Module),
    [false] = ehome_element:get_outputs(Module),

    ehome_element:set_input(Module, 1, true),
    test_utils:wait_queue_empty(Module),
    [true] = ehome_element:get_outputs(Module),

    ehome_element:set_input(Module, 1, false),
    test_utils:wait_queue_empty(Module),
    [true] = ehome_element:get_outputs(Module),

    ehome_element:set_input(Module, 2, true),
    test_utils:wait_queue_empty(Module),
    [false] = ehome_element:get_outputs(Module),

    ehome_element:set_input(Module, 2, false),
    test_utils:wait_queue_empty(Module),
    [false] = ehome_element:get_outputs(Module),

    ehome_dispatcher:stop().
