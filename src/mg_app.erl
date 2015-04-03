-module(ts_app).
-behaviour(application).
-export([start/2, stop/1]).

%%%===================================================================
%%% Callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    Enabled = true,
    case mg_sup:start_link(Enabled) of
        {ok, Pid} ->
            maybe_setup(Enabled),
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    % TODO: find pb codes that aren't taken
    % ok = riak_api_pb_service:deregister([{mg_pb_query, 70, 71}]),
    ok.

%% @private
maybe_setup(false) ->
  ok;
maybe_setup(true) ->
  setup_http(),
  setup_pb().

%% @private
setup_http() ->
    [webmachine_router:add_route(R) || R <- http_routes()].

%% @private
http_routes() ->
     [{["types", bucket_type, "buckets", bucket, "multi"],
      mg_wm_query, [{api_version, 3}]}].

%% @private
setup_pb() ->
    % TODO: find pb codes that aren't taken
    % ok = riak_api_pb_service:register([{mg_pb_query, 70, 71}]),
    ok.