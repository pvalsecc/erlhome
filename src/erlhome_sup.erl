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
    {ok, { {one_for_one, 5, 10}, [
        {change_notif, {gen_event, start_link, [{local, change_notif}]},
            permanent, 5000, worker, dynamic},
        {status_notif, {gen_event, start_link, [{local, status_notif}]},
            permanent, 5000, worker, dynamic},
        {elements_sup, {ehome_elements_sup, start_link, []},
            permanent, 5000, worker, [ehome_elements_sup]},
        {db, {ehome_db, start_link, [EnablePersistency]}, permanent, 5000, worker,
            [ehome_db]}
    ]}}.
