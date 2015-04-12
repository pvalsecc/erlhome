%%%-------------------------------------------------------------------
%%% @author patrick
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Apr 2015 17:28
%%%-------------------------------------------------------------------
-module(ehome_schemas).

%% API
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).

-export([create_schema/2]).
-export([schema_json/2]).


-record(schema, {id :: integer(), name :: binary()}).

init(Req, Opts) ->
    random:seed(now()),
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, schema_json}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, create_schema}],
        Req, State}.

resource_exists(Req, _State) ->
    case cowboy_req:binding(id, Req) of
        undefined ->
            {true, Req, index};
        ID ->
            case exists(ID) of
                true -> {true, Req, ID};
                false -> {false, Req, ID}
            end
    end.

create_schema(Req, State) ->
    %PasteID = new_paste_id(),
    %{ok, [{<<"paste">>, Paste}], Req2} = cowboy_req:body_qs(Req),
    %ok = file:write_file(full_path(PasteID), Paste),
    %case cowboy_req:method(Req2) of
    %    <<"POST">> ->
    %        {{true, <<$/, PasteID/binary>>}, Req2, State};
    %    _ ->
    %        {true, Req2, State}
    %end.
    {false, Req, State}.

schema_json(Req, index) ->
    {jiffy:encode([schema2json(#schema{id = 12, name = <<"toto">>})]), Req, index};
schema_json(Req, ID) ->
    {jiffy:encode(schema2json(#schema{id = 12, name = <<"toto">>})), Req, ID}.

%% Privates ===================

exists(12) -> true;
exists(_) -> false.

schema2json(#schema{id = ID, name = Name}) ->
    StrID = binary:list_to_bin(integer_to_list(ID)),
    #{id => ID, name => Name, href => <<"/schemas/", StrID/bits>>}.
