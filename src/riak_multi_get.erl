-module(riak_multi_get).
-export([query/3]).

%%%===================================================================
%%% API
%%%===================================================================

query(Type, Bucket, Keys) ->
    Client = client(),
    multi_fetch(Client, Type, Bucket, Keys).

%% @private
multi_fetch(Client, Type, Bucket, Keys) ->
    multi_fetch(Client, Type, Bucket, Keys, []).

multi_fetch(_Client, _Type, _Bucket, [], Accum) ->
    Accum;
multi_fetch(Client, Type, Bucket, [Key | Rest], Accum) ->
    case Client:get({Type, Bucket}, list_to_binary(integer_to_list(Key))) of
        {ok, Obj} ->
            Value = riak_object:get_value(Obj),
            multi_fetch(Client, Type, Bucket, Rest, [[{key, Key},{value, Value}] | Accum]);
        _ ->
            multi_fetch(Client, Type, Bucket, Rest, Accum)
    end.

%% @private
-spec client() -> any().
client() ->
    {ok,C} = riak:local_client(),
    C.