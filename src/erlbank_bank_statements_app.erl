%%%-------------------------------------------------------------------
%% @doc erlbank_bank_statements public API
%% @end
%%%-------------------------------------------------------------------

-module(erlbank_bank_statements_app).

-behaviour(application).

-export([start/2, stop/1]).


check_is_set(Var) ->
    case os:getenv(Var) of
        false ->
            io:format("Missing var ~s~n", [Var]),
            halt(1);
        _ -> ok
    end.

start_cowboy() ->
    %% Cowboy test code
    Dispatch = cowboy_router:compile([{'_', [{"/", web_frontend, index},
                                             {"/bank-statements/request", web_frontend, request_bank_statement}]}]),

    {ok, _} = cowboy:start_clear(my_http_listener,
                                 [{port, 8002}],
                                 #{env => #{dispatch => Dispatch}}).


start(_StartType, _StartArgs) ->
    database:init_database(),

    check_is_set("ACCOUNTS_HOST"),
    check_is_set("TRANSFERS_HOST"),

    lager:info("Starting bank-transfers service: ~p~n", [node()]),

    AccountNode = list_to_atom("accounts@" ++ os:getenv("ACCOUNTS_HOST")),
    TransferNode = list_to_atom("transfers@" ++ os:getenv("TRANSFERS_HOST")),

    eventhandler:start(AccountNode, TransferNode),

    start_cowboy(),
    erlbank_bank_statements_sup:start_link().

stop(_State) ->
    ok.

