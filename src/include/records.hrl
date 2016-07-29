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
    id, currency, balance, hold, available, profile_id
}).

%% Ledger entry
-record(coinbase_ledger, {
    id, created_at, amount, balance, type, details
}).
-record(coinbase_ledger_details, {
    order_id, trade_id, product_id
}).

%% Hold entry
-record(coinbase_hold, {
    id, account_id, created_at, updated_at, amount, type, ref
}).

%% Order entry
-record(coinbase_order, {
    id, product_id, side, stp, funds, specified_funds, type, post_only,
    created_at, fill_fees, filled_size, executed_value, status, settled, size,
    price, time_in_force
}).

%% Fill entry
-record(coinbase_fill, {
    trade_id, product_id, price, size, order_id, created_at, liquidity, fee,
    settled, side
}).
