%%%-------------------------------------------------------------------
%%% @author patrick
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. mai 2015 15:32
%%%-------------------------------------------------------------------
-module(ehome_mqtt_tree).
-author("patrick").

-behaviour(gen_server).

%% API
-export([start_link/0, dump/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(node, {
    value = undefined :: any(),
    subs = #{}:: #{any() => #node{}}
}).

-record(state, {
    mqtt :: pid(),
    root = #node{} :: #node{}
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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

dump() ->
    gen_server:cast(?MODULE, dump).

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
    Self = self(),
    Mqtt = mqtt_client:connect(),
    mqtt_client:subscribe(Mqtt, "zwave/get/#", fun(Topic, Message) ->
        gen_server:cast(Self, {from_mqtt, Topic, Message})
    end),
    ehome_dispatcher:subscribe([mqtt, set, all], self(),
        fun([mqtt, set | Topic], Value) ->
            gen_server:cast(Self, {to_mqtt, Topic, Value})
        end),
    {ok, #state{mqtt = Mqtt}}.

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
handle_cast({from_mqtt, TopicRaw, MessageRaw}, State) ->
    Topic = split_topic_name(TopicRaw),
    Message = mqtt2erlang(MessageRaw),
    {noreply, from_mqtt(Topic, Message, State)};
handle_cast({to_mqtt, Topic, Value}, #state{mqtt = Mqtt} = State) ->
    TopicMqtt = build_topic_name(Topic),
    Message = erlang2mqtt(Value),
    lager:info("toMQTT: ~s = ~p", [TopicMqtt, Value]),
    mqtt_client:publish(Mqtt, TopicMqtt, Message),
    {noreply, State};
handle_cast(dump, #state{root = Root} = State) ->
    dump(Root, ""),
    {noreply, State};
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

learn([], Value, #node{value = Value} = Root) ->
    {Root, false};
learn([], Value, Root) ->
    {Root#node{value = Value}, true};
learn([H|Rest], Value, #node{subs = Subs} = Root) ->
    Sub = maps:get(H, Subs, #node{}),
    {NewSub, Changed} = learn(Rest, Value, Sub),
    {Root#node{subs = maps:put(H, NewSub, Subs)}, Changed}.

dump(#node{subs = Subs}, Indent) ->
    NewIndent = "  " ++ Indent,
    maps:fold(fun(Key, #node{value = Value} = Node, _Acc) ->
        io:format("~s~p = ~p~n", [Indent, Key, Value]),
        dump(Node, NewIndent)
    end, ok, Subs).

mqtt2erlang(<<0:8>>) ->
    empty;
mqtt2erlang(<<1:8, 0:8>>) ->
    false;
mqtt2erlang(<<1:8, _:8>>) ->
    true;
mqtt2erlang(<<2:8, Val:32/integer-native>>) ->
    Val;
mqtt2erlang(<<3:8, Val:32/float-native>>) ->
    Val;
mqtt2erlang(<<4:8, Val/binary>>) ->
    binary_to_list(Val);
mqtt2erlang(<<5:8, Val/binary>>) ->
    Val;
mqtt2erlang(Message) ->
    Message.

erlang2mqtt(empty) ->
    <<0:8>>;
erlang2mqtt(false) ->
    <<1:8, 0:8>>;
erlang2mqtt(true) ->
    <<1:8, 1:8>>;
erlang2mqtt(Val) when is_integer(Val) ->
    <<2:8, Val:32/integer-native>>;
erlang2mqtt(Val) when is_float(Val) ->
    <<3:8, Val:32/float-native>>;
erlang2mqtt(Val) when is_list(Val) ->
    <<4:8, (list_to_binary(Val))/binary>>;
erlang2mqtt(Val) when is_binary(Val) ->
    <<5:8, Val/binary>>.

from_mqtt(["zwave", "get", "devices", Device, "instances", Instance,
           "commandClasses", Class, "data" | Rest], Message,
        #state{root = Root} = State) ->
    Path = to_path(Device, Instance, Class, Rest),
    case learn(Path, Message, Root) of
        {NewRoot, true} ->
            lager:debug("~p = ~p", [Path, Message]),
            ehome_dispatcher:publish([mqtt, get | Path], Message),
            State#state{root = NewRoot};
        {_, false} ->
            State
    end;
from_mqtt(Unknown, Message, State) ->
    lager:warning("Unknown message: ~p = ~p", [Unknown, Message]),
    State.

build_topic_name([Device, Instance, Class | Rest]) ->
    List = ["zwave", "set", "devices", integer_to_list(Device),
            "instances", integer_to_list(Instance), "commandClasses",
            name2class(Class), "data" | Rest],
    string:join(List, "/").

to_path(Device, Instance, Class, Rest) ->
    DeviceNum = list_to_integer(Device),
    InstanceNum = list_to_integer(Instance),
    ClassName = class2name(Class),
    [DeviceNum, InstanceNum, ClassName | Rest].

split_topic_name(Name) ->
    string:tokens(Name, "/").

class2name(Class) when is_list(Class) ->
    class2name(list_to_integer(Class));
class2name(Class) when is_integer(Class) ->
    case lists:keyfind(Class, 2, classes()) of
        false -> Class;
        {Name, Class} -> Name
    end.

name2class(Name) when is_atom(Name) ->
     case lists:keyfind(Name, 1, classes()) of
         {Name, Class} -> integer_to_list(Class);
         Other -> Other
     end;
name2class(Name) when is_integer(Name) ->
     integer_to_list(Name);
name2class(Name) when is_list(Name) ->
    Name.

classes() ->
    [
        {no_operation, 0},
        {basic, 32},
        {controller_replication, 33},
        {application_status, 34},
        {zip_services, 35},
        {zip_server, 36},
        {switch_binary, 37},
        {switch_multilevel, 38},
        {switch_multilevel_v2, 38},
        {switch_all, 39},
        {switch_toggle_binary, 40},
        {switch_toggle_multilevel, 41},
        {chimney_fan, 42},
        {scene_activation, 43},
        {scene_actuator_conf, 44},
        {scene_controller_conf, 45},
        {zip_client, 46},
        {zip_adv_services, 47},
        {sensor_binary, 48},
        {sensor_multilevel, 49},
        {sensor_multilevel_v2, 49},
        {meter, 50},
        {zip_adv_server, 51},
        {zip_adv_client, 52},
        {meter_pulse, 53},
        {meter_tbl_config, 60},
        {meter_tbl_monitor, 61},
        {meter_tbl_push, 62},
        {thermostat_heating, 56},
        {thermostat_mode, 64},
        {thermostat_operating_state, 66},
        {thermostat_setpoint, 67},
        {thermostat_fan_mode, 68},
        {thermostat_fan_state, 69},
        {climate_control_schedule, 70},
        {thermostat_setback, 71},
        {door_lock_logging, 76},
        {schedule_entry_lock, 78},
        {basic_window_covering, 80},
        {mtp_window_covering, 81},
        {multi_channel_v2, 96},
        {multi_instance, 96},
        {door_lock, 98},
        {user_code, 99},
        {barrier_operator, 102},
        {configuration, 112},
        {alarm, 113},
        {manufacturer_specific, 114},
        {powerlevel, 115},
        {protection, 117},
        {protection_v2, 117},
        {lock, 118},
        {node_naming, 119},
        {firmware_update_md, 122},
        {grouping_name, 123},
        {remote_association_activate, 124},
        {remote_association, 125},
        {battery, 128},
        {clock, 129},
        {hail, 130},
        {wake_up, 132},
        {wake_up_v2, 132},
        {association, 133},
        {association_v2, 133},
        {version, 134},
        {indicator, 135},
        {proprietary, 136},
        {language, 137},
        {time, 138},
        {time_parameters, 139},
        {geographic_location, 140},
        {composite, 141},
        {multi_channel_association_v2, 142},
        {multi_instance_association, 142},
        {multi_cmd, 143},
        {energy_production, 144},
        {manufacturer_proprietary, 145},
        {screen_md, 146},
        {screen_md_v2, 146},
        {screen_attributes, 147},
        {screen_attributes_v2, 147},
        {simple_av_control, 148},
        {av_content_directory_md, 149},
        {av_renderer_status, 150},
        {av_content_search_md, 151},
        {security, 152},
        {av_tagging_md, 153},
        {ip_configuration, 154},
        {association_command_configuration, 155},
        {sensor_alarm, 156},
        {silence_alarm, 157},
        {sensor_configuration, 158},
        {mark, 239},
        {non_interoperable, 240}
    ].

%%%===================================================================
%%% Tests
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

learn_test() ->
    {T1, true} = learn([a,b,c], 1, #node{}),
    {T2, true} = learn([a,b,d], 2, T1),
    {_, false} = learn([a,b,d], 2, T2),
    #node{subs =
        #{a := #node{subs =
            #{b := #node{subs =
                #{c := #node{value = 1},
                  d := #node{value = 2}}
            }}
        }}
    } = T2.
