-module(coinbase_api_ws_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SUPERVISOR, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

init([]) ->
    Procs = [{
      coinbase_api_ws,
      {coinbase_api_ws, start_link, []},
      temporary, 5000, worker, [coinbase_api_ws]
    }],
    {ok, {{simple_one_for_one, 10, 10}, Procs}}.
