-module(coinbase_api).
-compile([{parse_transform, lager_transform}]).
-include("include/records.hrl").

%% Public API
-export([connect/3]).
-export([list_accounts/1]).
-export([get_account/2]).
-export([account_history/2]).
-export([get_holds/2]).
-export([place_order/5, place_order/6]).
-export([cancel_all_orders/1, cancel_order/2]).
-export([get_orders/1, get_orders/2]).
-export([get_order/2]).
-export([get_fills/1,get_fills/2]).
-export([extract_data/1]).

%% Internals.
-export([start_link/4]).
-export([init/5]).

-record(state, {
    parent :: pid(),
    owner :: pid(),
    key :: any(),
    secret :: any(),
    passphrase :: any(),
    gun ::any()
}).

extract_data(#coinbase_api_resp{data=Data}) ->
    Data.

connect(Key, Secret, Passphrase) ->
    case supervisor:start_child(coinbase_api_sup, [self(), Key, Secret, Passphrase]) of
        OK = {ok, _} ->
            OK;
        StartError ->
            StartError
    end.

start_link(Owner, Key, Secret, Passphrase) ->
    proc_lib:start_link(?MODULE, init,
        [self(), Owner, Key, Secret, Passphrase]).

init(Parent, Owner, Key, Secret, Passphrase) ->
    ok = proc_lib:init_ack(Parent, {ok, self()}),
    {ok,Gun} = gun:open("api.gdax.com", 443, #{protocols => [http]}),
    loop(#state{
           parent=Parent,
           owner=Owner,
           key=Key,
           secret=Secret,
           passphrase=Passphrase,
           gun=Gun
          }).

%% Get a list of trading accounts
list_accounts(ServerPid) ->
    request(get_accounts, ServerPid, <<"GET">>, <<"/accounts">>).

%% Information for a single account. Use this endpoint when you know the
%% account_id
get_account(ServerPid, Account) when is_binary(Account) ->
    request(get_account, ServerPid, <<"GET">>, <<"/accounts/", Account/binary>>).

%% Get Holds. Holds are placed on an account for any active orders or pending
%% withdraw requests. As an order is filled, the hold amount is updated. If an
%% order is canceled, any remaining hold is removed. For a withdraw, once it is
%% completed, the hold is removed.
get_holds(ServerPid, Account) when is_binary(Account) ->
    request(get_holds, ServerPid, <<"GET">>, <<"/accounts/", Account/binary, "/holds">>).
get_holds(ServerPid, Account, {Paginate, #coinbase_api_resp{cb_before=Before, cb_after=After}}) ->
    URI = case Paginate
    of before -> <<"/accounts/", Account/binary, "/holds/?before=", Before/binary>>;
       since  -> <<"/accounts/", Account/binary, "/holds/?after=", After/binary>>
    end,
    request(get_fills, ServerPid, <<"GET">>, URI).

%% Place order
place_order(ServerPid, limit, Side, Product, Price, Size) ->
    Json = jsx:encode([
       {<<"type">>, <<"limit">>},
       {<<"side">>, Side},
       {<<"product_id">>, Product},
       {<<"price">>, decimal:as_binary(Price)},
       {<<"size">>, decimal:as_binary(Size)}
    ]),
    request(place_order, ServerPid, <<"POST">>, <<"/orders">>, Json).
place_order(ServerPid, market, Side, Product, {size, Size}) ->
    Json = jsx:encode([
       {<<"type">>, <<"market">>},
       {<<"side">>, Side},
       {<<"product_id">>, Product},
       {<<"size">>, decimal:as_binary(Size)}
    ]),
    request(place_order, ServerPid, <<"POST">>, <<"/orders">>, Json);
place_order(ServerPid, market, Side, Product, {funds, Funds}) ->
    Json = jsx:encode([
       {<<"type">>, <<"market">>},
       {<<"side">>, Side},
       {<<"product_id">>, Product},
       {<<"funds">>, decimal:as_binary(Funds)}
    ]),
    request(place_order, ServerPid, <<"POST">>, <<"/orders">>, Json).

parse_order(Binary) when is_binary(Binary) ->
    parse_order(jsx:decode(Binary));
parse_order(Proplist) when is_list(Proplist) ->
    #coinbase_order{
        id = proplists:get_value(<<"id">>, Proplist),
        product_id = proplists:get_value(<<"product_id">>, Proplist),
        side = proplists:get_value(<<"side">>, Proplist),
        stp = proplists:get_value(<<"stp">>, Proplist),
        funds = decimal:from_binary(proplists:get_value(<<"funds">>, Proplist, <<"0">>)),
        specified_funds = decimal:from_binary(proplists:get_value(<<"specified_funds">>, Proplist, <<"0">>)),
        type = proplists:get_value(<<"type">>, Proplist),
        post_only = proplists:get_value(<<"post_only">>, Proplist),
        created_at = proplists:get_value(<<"created_at">>, Proplist),
        fill_fees = decimal:from_binary(proplists:get_value(<<"fill_fees">>, Proplist, <<"0">>)),
        filled_size = decimal:from_binary(proplists:get_value(<<"filled_size">>, Proplist, <<"0">>)),
        executed_value = decimal:from_binary(proplists:get_value(<<"executed_value">>, Proplist, <<"0">>)),
        status = proplists:get_value(<<"status">>, Proplist),
        settled = proplists:get_value(<<"settled">>, Proplist),
        size = decimal:from_binary(proplists:get_value(<<"size">>, Proplist, <<"0">>)),
        price = decimal:from_binary(proplists:get_value(<<"price">>, Proplist, <<"0">>)),
        time_in_force = proplists:get_value(<<"time_in_force">>, Proplist)
      };
parse_order(Anything) -> Anything.

parse_order_list(Binary) when is_binary(Binary) ->
    OrderList = jsx:decode(Binary),
    [ parse_order(Order) || Order <- OrderList ].

%% Get the list of current open orders. Only open or un-settled orders are
%% returned. As soon as an order is no longer open and settled, it will no
%% longer appear in the default request.
get_orders(ServerPid) ->
    request(get_orders, ServerPid, <<"GET">>, <<"/orders">>).
get_orders(ServerPid, open) ->
    request(get_orders, ServerPid, <<"GET">>, <<"/orders?status=open">>);
get_orders(ServerPid, pending) ->
    request(get_orders, ServerPid, <<"GET">>, <<"/orders?status=pending">>);
get_orders(ServerPid, active) ->
    request(get_orders, ServerPid, <<"GET">>, <<"/orders?status=active">>);
get_orders(ServerPid, all) ->
    request(get_orders, ServerPid, <<"GET">>, <<"/orders?status=all">>);
get_orders(ServerPid, done) ->
    request(get_orders, ServerPid, <<"GET">>, <<"/orders?status=done">>).

%% Get a single order by ID
get_order(ServerPid, OrderId) ->
    request(get_order, ServerPid, <<"GET">>, <<"/orders/", OrderId/binary>>).

%% Cancel a single order
cancel_order(ServerPid, OrderId) when is_binary(OrderId) ->
    request(cancel_order, ServerPid, <<"DELETE">>, <<"/orders/", OrderId/binary>>).

%% Cancel all open orders
cancel_all_orders(ServerPid) ->
    request(cancel_order, ServerPid, <<"DELETE">>, <<"/orders">>).

%% Parses a JSON representation of a coinbase account into a record
parse_account(Json) when is_binary(Json) ->
    AccountJson = jsx:decode(Json),
    parse_account(AccountJson);
parse_account(AccountProplist) when is_list(AccountProplist) ->
    #coinbase_account{
        id = proplists:get_value(<<"id">>, AccountProplist),
        currency = proplists:get_value(<<"currency">>, AccountProplist),
        balance = decimal:from_binary(proplists:get_value(<<"balance">>, AccountProplist)),
        hold = decimal:from_binary(proplists:get_value(<<"hold">>, AccountProplist)),
        available = decimal:from_binary(proplists:get_value(<<"available">>, AccountProplist)),
        profile_id = proplists:get_value(<<"profile_id">>, AccountProplist)
    };
parse_account(Unknown) -> Unknown.

%% Parses a JSON list of coinbase accounts
parse_list_accounts(Json) when is_binary(Json) ->
    DecodedJson = jsx:decode(Json),
    AccountRecords = [
        parse_account(AccountJson) || AccountJson <- DecodedJson
    ],
    AccountRecords;
parse_list_accounts(Unknown) -> Unknown.

%% List account activity. Account activity either increases or decreases your
%% account balance. Items are paginated and sorted latest first.
account_history(ServerPid, Account) when is_binary(Account) ->
    request(account_history, ServerPid, <<"GET">>, <<"/accounts/", Account/binary, "/ledger">>).
account_history(ServerPid, Account, {Paginate, #coinbase_api_resp{cb_before=Before, cb_after=After}}) ->
    URI = case Paginate
    of before -> <<"/fills/", Account/binary, "/ledger?before=", Before/binary>>;
       since  -> <<"/fills/", Account/binary, "/ledger/?after=", After/binary>>
    end,
    request(get_fills, ServerPid, <<"GET">>, URI).

%% Parses ledger entry JSON to a record. Accepts strings or proplists.
parse_ledger_entry(Entry) when is_binary(Entry) ->
    parse_ledger_entry(jsx:decode(Entry));
parse_ledger_entry(Entry) when is_list(Entry) ->
    Details = proplists:get_value(<<"details">>, Entry),
    #coinbase_ledger{
        id = proplists:get_value(<<"id">>, Entry),
        created_at =proplists:get_value(<<"created_at">>, Entry),
        amount = decimal:from_binary(proplists:get_value(<<"amount">>, Entry)),
        balance = decimal:from_binary(proplists:get_value(<<"balance">>, Entry)),
        type =proplists:get_value(<<"type">>, Entry),
        details = #coinbase_ledger_details{
            order_id =proplists:get_value(<<"order_id">>, Details),
            trade_id =proplists:get_value(<<"trade_id">>, Details),
            product_id =proplists:get_value(<<"product_id">>, Details)
        }
    }.

%% Parses JSON a list of ledger entries
parse_ledger_entry_list(EntryList) when is_binary(EntryList) ->
    DecodedEntry = jsx:decode(EntryList),
    [ parse_ledger_entry(Entry) || Entry <- DecodedEntry ].

%% Parses a hold entry (JSON or proplist) to a record
parse_hold_entry(Entry) when is_binary(Entry) ->
    parse_hold_entry(jsx:decode(Entry));
parse_hold_entry(Entry) when is_list(Entry) ->
    #coinbase_hold{
        id = proplists:get_value(<<"id">>, Entry),
        account_id = proplists:get_value(<<"account_id">>, Entry),
        created_at =proplists:get_value(<<"created_at">>, Entry),
        updated_at =proplists:get_value(<<"updated_at">>, Entry),
        amount = decimal:from_binary(proplists:get_value(<<"amount">>, Entry)),
        type =proplists:get_value(<<"type">>, Entry),
        ref =proplists:get_value(<<"ref">>, Entry)
    }.

%% Parses a JSON list of hold entries
parse_hold_entry_list(EntryList) when is_binary(EntryList) ->
    DecodedEntry = jsx:decode(EntryList),
    [ parse_hold_entry(Entry) || Entry <- DecodedEntry ].

%% Get a list of fills
get_fills(ServerPid) ->
    request(get_fills, ServerPid, <<"GET">>, <<"/fills/">>).
get_fills(ServerPid, {before, #coinbase_api_resp{cb_before=CB}}) ->
    request(get_fills, ServerPid, <<"GET">>, <<"/fills/?before=", CB/binary>>);
get_fills(ServerPid, {since, #coinbase_api_resp{cb_after=CB}}) ->
    request(get_fills, ServerPid, <<"GET">>, <<"/fills/?after=", CB/binary>>).

%% Parses fill JSON to a record. Accepts strings or proplists.
parse_fill_entry(Entry) when is_binary(Entry) ->
    parse_fill_entry(jsx:decode(Entry));
parse_fill_entry(Entry) when is_list(Entry) ->
    #coinbase_fill{
        id = proplists:get_value(<<"id">>, Entry),
        product_id = proplists:get_value(<<"product_id">>, Entry),
        price = decimal:from_binary(proplists:get_value(<<"price">>, Entry)),
        size = decimal:from_binary(proplists:get_value(<<"size">>, Entry)),
        order_id = proplists:get_value(<<"order_id">>, Entry),
        created_at = proplists:get_value(<<"created_at">>, Entry),
        liquidity = proplists:get_value(<<"liquidity">>, Entry),
        fee = decimal:from_binary(proplists:get_value(<<"fee">>, Entry)),
        settled = proplists:get_value(<<"settled">>, Entry),
        side = proplists:get_value(<<"side">>, Entry)
    }.

%% Parses JSON a list of fill entries
parse_fill_entry_list(EntryList) when is_binary(EntryList) ->
    DecodedEntry = jsx:decode(EntryList),
    [ parse_fill_entry(Entry) || Entry <- DecodedEntry ].

%% Utility function for passing requests to the server process
request(RequestType, ServerPid, Method, Endpoint) ->
    request(RequestType, ServerPid, Method, Endpoint, <<"">>).
request(RequestType, ServerPid, Method, Endpoint, Body) ->
    Ref = make_ref(),
    ServerPid ! {RequestType, self(), Ref, Method, Endpoint, Body},
    receive {Ref, Answer} -> Answer
    after 5000 -> {error, timeout}
    end.

%% Handle the actual signing, headers and response of a coinbase request.
api_request(#state{gun=Gun, key=Key, secret=Secret, passphrase=Passphrase}, Method, Path, Body) ->
    Timestamp = erlang:integer_to_binary(os:system_time() div 1000000000),
    Data = <<Timestamp/binary, Method/binary, Path/binary, Body/binary>>,
    HmacKey = base64:decode(Secret),
    Signature = crypto:hmac(sha256, HmacKey, Data),
    Signature64 = base64:encode(Signature),
    Headers = [
        {<<"CB-ACCESS-KEY">>, Key},
        {<<"CB-ACCESS-SIGN">>, Signature64},
        {<<"CB-ACCESS-TIMESTAMP">>, Timestamp},
        {<<"CB-ACCESS-PASSPHRASE">>, Passphrase},
        {<<"Content-Type">>, <<"application/json">>},
        {<<"User-Agent">>, <<"erlang:gun">>}
    ],
    StreamRef = case Method of <<"GET">> -> gun:get(Gun, Path, Headers);
                               <<"POST">> -> gun:post(Gun, Path, Headers, Body);
                               <<"DELETE">> -> gun:delete(Gun, Path, Headers)
                end,
    Awaited =
    case gun:await(Gun, StreamRef) of
        {response, fin, Status, ResponseHeaders} ->
            {Status, no_data};
        {response, nofin, Status, ResponseHeaders} ->
            {ok, ResponseBody} = gun:await_body(Gun, StreamRef),
            {Status, ResponseBody, ResponseHeaders};
        {error, timeout} ->
            timeout;
        Anything ->
            lager:info("Got unknown response: ~p~n", [Anything]),
            error
    end.

call_and_parse_response(State, Method, Path, Body, Parser) ->
    case api_request(State, Method, Path, Body) of
        {200, Response, Headers} ->
            {ok, #coinbase_api_resp{
                data=apply(Parser, [Response]),
                cb_before=proplists:get_value(<<"cb-before">>, Headers),
                cb_after=proplists:get_value(<<"cb-after">>, Headers)
            }};
        Other -> {error, Other}
    end.

loop(State=#state{}) ->
    receive
        {get_accounts, Owner, Ref, Method, Path, Body} ->
            Owner ! {
              Ref,
              call_and_parse_response(
                State, Method, Path, Body, fun parse_list_accounts/1
            )},
            loop(State);
        {get_account, Owner, Ref, Method, Path, Body} ->
            Owner ! {
              Ref,
              call_and_parse_response(
                State, Method, Path, Body, fun parse_account/1
            )},
            loop(State);
        {account_history, Owner, Ref, Method, Path, Body} ->
            Owner ! {
              Ref,
              call_and_parse_response(
                State, Method, Path, Body, fun parse_ledger_entry_list/1
            )},
            loop(State);
        {get_holds, Owner, Ref, Method, Path, Body} ->
            Owner ! {
              Ref,
              call_and_parse_response(
                State, Method, Path, Body, fun parse_hold_entry_list/1
            )},
            loop(State);
        {place_order, Owner, Ref, Method, Path, Body} ->
            Owner ! {
              Ref,
              call_and_parse_response(
                State, Method, Path, Body, fun parse_order/1
            )},
            loop(State);
        {cancel_order, Owner, Ref, Method, Path, Body} ->
            Owner ! {
              Ref,
              call_and_parse_response(
                State, Method, Path, Body, fun jsx:decode/1
            )},
            loop(State);
        {get_orders, Owner, Ref, Method, Path, Body} ->
            Owner ! {
              Ref,
              call_and_parse_response(
                State, Method, Path, Body, fun parse_order_list/1
            )},
            loop(State);
        {get_order, Owner, Ref, Method, Path, Body} ->
            Owner ! {
              Ref,
              call_and_parse_response(
                State, Method, Path, Body, fun parse_order/1
            )},
            loop(State);
        {get_fills, Owner, Ref, Method, Path, Body} ->
            Owner ! {
              Ref,
              call_and_parse_response(
                State, Method, Path, Body, fun parse_fill_entry_list/1
            )},
            loop(State);
        {gun_up, Gun, http} ->
            loop(State#state{gun=Gun});
        {gun_down, _, _, _, _, _} ->
            loop(State);
        Anything ->
            lager:info("Got message: ~p~n", [Anything]),
            loop(State)
    end.
