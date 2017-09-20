%% @private
-module(coinbase_api_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% supervisor.

init([]) ->
    Children = [
    {
        coinbase_api_http_sup,
        {coinbase_api_http_sup, start_link, []},
        permanent,
        3000,
        supervisor,
        [coinbase_api_http_sup]
    },
    {
        coinbase_api_ws_sup,
        {coinbase_api_ws_sup, start_link, []},
        permanent,
        3000,
        supervisor,
        [coinbase_api_ws_sup]
    }
    ],
    {ok, {{one_for_one, 11, 10}, Children }}.
