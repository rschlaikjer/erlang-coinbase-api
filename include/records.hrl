%%%
%%% Record definitions for the coinbase exchange API.
%%%
%%%

%% Record for a coinbase API response.
%% If the response is for a paginated API, cb_before and cb_after can be used
%% to request different pages.
-record(coinbase_api_resp, {
    data :: any(),
    cb_before,
    cb_after
}).

%% Account struct
-record (coinbase_account, {
    id,
    currency,
    balance=decimal:new(0,0),
    hold,
    available,
    profile_id
}).

%% Ledger entry
-record(coinbase_ledger, {
    id,
    created_at,
    amount=decimal:new(0,0),
    balance,
    type,
    details
 }).
-record(coinbase_ledger_details, {
    order_id,
    trade_id,
    product_id
 }).

%% Hold entry
-record(coinbase_hold, {
    id,
    account_id,
    created_at,
    updated_at,
    amount=decimal:new(0,0),
    type,
    ref
}).

%% Order entry
-record(coinbase_order, {
    id,
    product_id,
    side,
    stp,
    funds=decimal:new(0,0),
    specified_funds=decimal:new(0,0),
    type,
    post_only,
    created_at,
    fill_fees=decimal:new(0,0),
    filled_size=decimal:new(0,0),
    executed_value,
    status,
    settled,
    size=decimal:new(0,0),
    price=decimal:new(0,0),
    time_in_force
}).

%% Fill entry
-record(coinbase_fill, {
    product_id,
    price=decimal:new(0,0),
    size=decimal:new(0,0),
    order_id,
    created_at,
    liquidity,
    fee=decimal:new(0,0),
    settled,
    side
}).
