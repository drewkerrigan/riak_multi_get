-module(mg_wm_query).
-export([
    init/1,
    service_available/2,
    allowed_methods/2,
    content_types_provided/2,
    malformed_request/2,
    resource_exists/2,
    to_json/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, {
        type,
        bucket,
        start,
        stop
    }).

%%%===================================================================
%%% Callbacks
%%%===================================================================

init(_) -> 
    {ok, #ctx{}}.

service_available(ReqData, Context=#ctx{}) ->
    {
        true,
        ReqData,
        Context
    }.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.

malformed_request(ReqData, Context) ->
    case context_from(ReqData, Context) of
        {error, malformed_query} -> {true, ReqData, Context};
        PopulatedContext -> {false, ReqData, PopulatedContext}
    end.

resource_exists(ReqData, Context) ->
    {true, ReqData, Context}.


to_json(ReqData, Context) ->
    Options = [{start, Context#ctx.start},{stop, Context#ctx.stop}],
    Results = riak_multi_get:query(Context#ctx.type, Context#ctx.bucket, Options),
    JsonResults = mochijson2:encode([{count, length(Results)},{results, Results}]),

    {
        JsonResults,
        ReqData,
        Context
     }.

%% @private
context_from(ReqData, Context) ->
    try
        Type = list_to_binary(wrq:path_info(bucket_type, ReqData)),
        Bucket = list_to_binary(wrq:path_info(bucket, ReqData)),
        KeyStr = list_to_integer(wrq:get_qs_value("keys",ReqData)),
        Keys = string:tokens(KeyStr,","),

        Context#ctx{
            type = Type,
            bucket = Bucket,
            keys = Keys
        }
    catch
        Exception:Reason ->
            lager:debug("Malformed query: ~p. ~p:~p", [wrq:req_body(ReqData), Exception, Reason]),
            {error, malformed_query}
    end.