%%%-------------------------------------------------------------------
%% @doc erlbank_bank_statements public API
%% @end
%%%-------------------------------------------------------------------

-module(erlbank_bank_statements_app).

-behaviour(application).

-export([start/2, stop/1]).




start_cowboy() ->
    %% Cowboy test code
    Dispatch = cowboy_router:compile([{'_', [{"/", web_frontend, index},
                                             {"/bank-statements/request", web_frontend, request_bank_statement}]}]),

    {ok, _} = cowboy:start_clear(my_http_listener,
                                 [{port, 8002}],
                                 #{env => #{dispatch => Dispatch}}).


start(_StartType, _StartArgs) ->
    database:init_database(),

    lager:info("Starting bank-transfers service: ~p~n", [node()]),

    start_cowboy(),
    erlbank_bank_statements_sup:start_link().

stop(_State) ->
    ok.

