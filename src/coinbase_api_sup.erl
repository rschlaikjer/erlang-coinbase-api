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
	Procs = [{coinbase_api, {coinbase_api, start_link, []},
		temporary, 5000, worker, [coinbase_api]}],
	{ok, {{simple_one_for_one, 10, 10}, Procs}}.
