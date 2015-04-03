-module(ts_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).


%%%===================================================================
%%% API
%%%===================================================================

start_link(Enabled) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Enabled]).


%%%===================================================================
%%% Callbacks
%%%===================================================================

init([false]) ->
    {ok, {{one_for_one, 5, 10}, []}};

init([_Enabled]) ->
    {ok, {{one_for_one, 5, 10}, []}}.