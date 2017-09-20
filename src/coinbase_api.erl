-module(coinbase_api).

%% Public API
-export([
    connect_http/3,
    connect_ws/0
]).

connect_http(Key, Secret, Passphrase) ->
    supervisor:start_child(
        coinbase_api_http_sup,
        [self(), Key, Secret, Passphrase]
    ).

connect_ws() ->
    supervisor:start_child(coinbase_api_ws_sup, [self()]).
