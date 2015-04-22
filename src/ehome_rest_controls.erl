%%%-------------------------------------------------------------------
%%% @author pvalsecc
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Apr 2015 8:55 AM
%%%-------------------------------------------------------------------
-module(ehome_rest_controls).
-author("pvalsecc").

%% API
-export([init/2, allowed_methods/2, content_types_accepted/2, from_json/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"PUT">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, from_json}],
        Req, State}.

from_json(Req, State) ->
    {ok, Json, Req2} = cowboy_req:body(Req),
    Id = cowboy_req:binding(element_id, Req2),
    Type = cowboy_req:binding(type, Req2),
    Message = jiffy:decode(Json, [return_maps]),
    {ehome_elements_sup:control(Id, Type, Message), Req2, State}.
