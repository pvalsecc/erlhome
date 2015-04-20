-module(erlhome_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
        {change_notif, {gen_event, start_link, [{local, change_notif}]},
            permanent, 5000, worker, dynamic},
        {db, {ehome_db, start_link, []}, permanent, 5000, worker, [ehome_db]},
        {elements_sup, {ehome_elements_sup, start_link, []},
            permanent, 5000, worker, [ehome_elements_sup]}
    ]}}.
