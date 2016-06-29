# erlang-coinbase-api
## Coinbase exchange API in Erlang

Enough features for basic usage. Missing pagination support.

## Basic Usage

Create an API client:

    {ok, Coinbase} = coinbase_api:connect(
        <<"Api Key">>,
        <<"Api Secret">>,
        <<"Api Passphrase">>
    ).

List exchange accounts:

    coinbase_api:list_accounts(Coinbase).

        {ok,[{coinbase_account,<<"Account UUID">>,
                           <<"BTC">>,
                           {decimal,1947978100000000,16},
                           {decimal,0,16},
                           {decimal,1947978100000000,16},
                           <<"Profile UUID">>},
         {coinbase_account,<<"Account UUID">>,
                           <<"USD">>,
                           {decimal,3242303754453185000,16},
                           {decimal,0,16},
                           {decimal,3242303754453185000,16},
                           <<"Profile UUID">>},
         {coinbase_account,<<"Account UUID">>,
                           <<"ETH">>,
                           {decimal,0,16},
                           {decimal,0,16},
                           {decimal,0,16},
                           <<"Profile UUID">>}]}

Account history:

    coinbase_api:account_history(Coinbase, <<"Account UUID">>).

        {ok,[{coinbase_ledger,63965202,
                          <<"2016-06-29T02:40:41.098052Z">>,
                          {decimal,78042300000000,16},
                          {decimal,1947978100000000,16},
                          <<"match">>,
                          {coinbase_ledger_details,<<"Order ID">>,
                                                   <<"Trade ID">>,<<"BTC-USD">>}},
         {coinbase_ledger,63965195,<<"2016-06-29T02:40:40.991891Z">>,
                          {decimal,100000000000000,16},
                          {decimal,1869935800000000,16},
                          <<"match">>,
                          {coinbase_ledger_details,<<"Order ID">>,
                                                   <<"Trade ID">>,<<"BTC-USD">>}},
         {coinbase_ledger,63306049,<<"2016-06-24T16:34:31.75055Z">>,
                          {decimal,2000000000000000,16},
                          {decimal,1969935800000000,16},
                          <<"match">>,
                          {coinbase_ledger_details,<<"Order ID">>,
                                                   <<"Trade ID">>,<<"BTC-USD">>}},

List Holds:

    coinbase_api:get_holds(Coinbase, <<"7ec9e36e-bc0d-4e3a-8d85-a6dc9a3587e9">>).

Place limit order:

    coinbase_api:place_order(Coinbase, limit, <<"sell">>, <<"BTC-USD">>, decimal:new(1000, 0), decimal:new(1, 2)).
      {ok,{coinbase_order,<<"6cfb378c-1075-4c2b-be20-2359b6185e09">>,
                          <<"BTC-USD">>,<<"sell">>,<<"dc">>,undefined,undefined,
                          <<"limit">>,false,<<"2016-06-29T02:45:31.846734Z">>,
                          {decimal,0,16},
                          {decimal,0,8},
                          {decimal,0,16},
                          <<"pending">>,false,
                          {decimal,1000000,8},
                          {decimal,100000000000,8},
                          <<"GTC">>}}

Place market order:

    coinbase_api:place_order(Coinbase, market, <<"sell">>, <<"BTC-USD">>, {size, decimal:new(1, 2)}).

Cancel open order:

    coinbase_api:cancel_order(Coinbase, <<"fb4c86bb-65cd-4976-b467-d8762821fc4b">>).

Cancel all open orders:

    coinbase_api:cancel_all_orders(Coinbase).

List open orders:

    coinbase_api:get_orders(Coinbase).

List orders in any state:

    coinbase_api:get_orders(Coinbase, all).

Get specific order:

    coinbase_api:get_order(Coinbase, <<"e5a88435-2dc2-4793-bd3f-b041d8aa230c">>).

Get all fills:

    coinbase_api:get_fills(Coinbase).
