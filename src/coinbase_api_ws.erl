-module(coinbase_api_ws).
-behaviour(gen_server).
-include("include/records.hrl").

-compile([{parse_transform, lager_transform}]).

-define(WS_URI, "wss://ws-feed.gdax.com").

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    callback :: pid(),
    gun :: any()
}).

start_link(Callback) ->
    gen_server:start_link(?MODULE, [Callback], []).

init([Callback]) ->
    State = #state{
        callback=Callback
    },
    gen_server:cast(self(), reconnect),
    {ok, State}.

handle_call(Request, _From, State) ->
    lager:info("Unexpected call ~p~n", [Request]),
    {noreply, State}.

handle_cast(reconnect, State) ->
    {ok, State1} = reconnect_websocket(State),
    {noreply, State1};
handle_cast(Msg, State) ->
    lager:info("Unexpected cast ~p~n", [Msg]),
    {noreply, State}.

handle_info({gun_ws, Gun, close}, State) ->
    lager:info("WS state -> closed"),
    gun:close(Gun),
    gen_server:cast(self(), reconnect),
    {noreply, State};
handle_info({gun_ws, _Gun, Message}, State) ->
    handle_ws_json_message(State, Message),
    {noreply, State};
handle_info({gun_up, _Gun, _Proto}, State) ->
    {noreply, State};
handle_info({gun_down, Gun, _Proto, Reason, [], []}, State) ->
    lager:info("Gun down (reason: ~p)~n", [Reason]),
    gun:close(Gun),
    gen_server:cast(self(), reconnect),
    {noreply, State};
handle_info({gun_ws_upgrade, Gun, ok, _Headers}, State) ->
    send_ws_message(Gun, subscribe_message()),
    {noreply, State};

handle_info(Info, State) ->
    lager:info("Unexpected info ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(OldVsn, State, _Extra) ->
    lager:info("~p updated from vsn ~p", [?MODULE, OldVsn]),
    {ok, State}.


%% Internal methods

reconnect_websocket(State) ->
    {ok, {wss, [], Host, 443, Fragment, []}} = http_uri:parse(
        ?WS_URI, [{scheme_defaults, [{wss, 443}]}]
    ),
    {ok, Pid} = gun:open(Host, 443, #{protocols => [http]}),
    StreamRef = gun:ws_upgrade(Pid, Fragment),
    {ok, State#state{gun=Pid}}.

subscribe_message() ->
    [
     {type, subscribe},
     {product_ids, [
        <<"BTC-USD">>, <<"LTC-USD">>, <<"ETH-USD">>
     ]},
     {channels, [
        <<"heartbeat">>,
        [{name, <<"ticker">>},
         {product_ids, [<<"BTC-USD">>, <<"LTC-USD">>, <<"ETH-USD">>]}]
     ]}
    ].

send_ws_message(Gun, Message) ->
    JsonMessage = jsx:encode(Message),
    lager:info("Sending: ~p~n", [JsonMessage]),
    gun:ws_send(Gun, {text, JsonMessage}).

handle_ws_json_message(State, {text, Message}) ->
    DecodedMessage = jsx:decode(Message),
    handle_ws_message(
        State,
        parse_ws_message(
            proplists:get_value(<<"type">>, DecodedMessage),
            DecodedMessage
        )
    ).

handle_ws_message(State, undefined) ->
    % Ignore messages that can't be parsed
    ok;
handle_ws_message(State, Message) ->
    Callback = State#state.callback,
    Callback ! {coinbase_api_ws, self(), Message},
    ok.

parse_ws_message(<<"heartbeat">>, Message) ->
    #coinbase_ws_heartbeat{
        sequence=proplists:get_value(<<"sequence">>, Message),
        last_trade_id=proplists:get_value(<<"last_trade_id">>, Message),
        product_id=proplists:get_value(<<"product_id">>, Message),
        time=proplists:get_value(<<"time">>, Message)
    };
parse_ws_message(<<"ticker">>, Message) ->
    #coinbase_ws_ticker{
        trade_id=proplists:get_value(<<"trade_id">>, Message),
        sequence=proplists:get_value(<<"sequence">>, Message),
        time=proplists:get_value(<<"time">>, Message),
        product_id=proplists:get_value(<<"product_id">>, Message),
        price=decimal:from_binary(proplists:get_value(<<"price">>, Message)),
        side =proplists:get_value(<<"side">>, Message),
        last_size=decimal:from_binary(proplists:get_value(<<"last_size">>, Message)),
        best_bid=decimal:from_binary(proplists:get_value(<<"best_bid">>, Message)),
        best_ask=decimal:from_binary(proplists:get_value(<<"best_ask">>, Message))
    };
parse_ws_message(Type, Message) ->
    lager:info("Got unhandled message type '~s': ~p~n", [Type, Message]),
    undefined.
