-module(erlhome_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(EnablePersistency) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [EnablePersistency]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([EnablePersistency]) ->
    Childs = [
        {dispatcher, {ehome_dispatcher, start_link, []},
            permanent, 5000, worker, dynamic},
        {elements_sup, {ehome_elements_sup, start_link, []},
            permanent, 5000, worker, [ehome_elements_sup]},
        {db, {ehome_db, start_link, [EnablePersistency]}, permanent, 5000, worker,
            [ehome_db]},
        {names, {ehome_map_service, start_link, [EnablePersistency, names]},
            permanent,
            5000, worker, [ehome_map_service]},
        {mqtt_tree, {ehome_mqtt_tree, start_link, []},
                         permanent, 5000, worker, [ehome_mqtt_tree]}
    ],
    {ok, { {rest_for_one, 5, 10}, Childs}}.
