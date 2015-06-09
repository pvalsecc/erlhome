%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Jun 2015 8:57 AM
%%%-------------------------------------------------------------------
-module(ehome_rest_mqtt_tree).
-author("pvalsecc").

%% API
-export([init/2, allowed_methods/2, content_types_provided/2, to_json/2, resource_exists/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, to_json}
    ], Req, State}.

resource_exists(Req, _State) ->
    case cowboy_req:parse_qs(Req) of
        [{<<"filter">>, Filter}] ->
            case ehome_utils:parse_path(Filter) of
                undefined -> {false, Req, index};
                FilterP -> {true, Req, FilterP}
            end;
        _ ->
            lager:warning("Invalid params"),
            {false, Req, index}
    end.

to_json(Req, Filter) ->
    {jiffy:encode(ehome_mqtt_tree:list_filter(Filter)), Req, Filter}.
