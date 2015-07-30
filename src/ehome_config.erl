%%%-------------------------------------------------------------------
%%% @author patrick
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. juil. 2015 15:09
%%%-------------------------------------------------------------------
-module(ehome_config).
-author("patrick").

%% API
-export([pan04/1, add/1, remove/1]).

config(DeviceId, Param, Value) ->
    ehome_dispatcher:publish([mqtt, set, DeviceId, 0, configuration, Param], Value, false).

control(Command, Param) ->
    ehome_dispatcher:publish([mqtt, control, Command], Param, false).

interview_force(DeviceId) ->
    control("interview_force", DeviceId).

pan04(DeviceId) ->
    config(DeviceId, "7", 0), %off when power back on
    config(DeviceId, "10", 2), %no Endpoint 3
    interview_force(DeviceId).

add(Switch) ->
    control("add_node", Switch).

remove(Switch) ->
    control("remove_node", Switch).