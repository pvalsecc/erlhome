-module(erlhome_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, boot/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/schemas/[:id]", [{id, int}], ehome_rest_schemas, []},
            {"/schemas/:schema_id/elements/[:sub_id]",
                [{schema_id, int}, {sub_id, int}], ehome_rest_elements, []},
            {"/schemas/:schema_id/connections/[:sub_id]",
                [{schema_id, int}, {sub_id, int}], ehome_rest_connections, []},
            {"/controls/:type/:element_id", [{element_id, int}],
                ehome_rest_controls, []},
            {"/notifs", ehome_ws_notifs, []},
            {"/", cowboy_static, {priv_file, erlhome, "assets/index.html"}},
            {"/[...]", cowboy_static, {priv_dir, erlhome, "assets",
                [{mimetypes, cow_mimetypes, all}]}}

        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
        {env, [{dispatch, Dispatch}]}
    ]),
    erlhome_sup:start_link().

stop(_State) ->
    ok.

boot() ->
    application:ensure_all_started(erlhome).
