-module(database_test).
-include_lib("eunit/include/eunit.hrl").
-include("data.hrl").

setup() ->
    database:init_database().

cleanup(_) -> ok.

main_test_() ->
    {inorder,
     {foreach,
      fun setup/0,
      fun cleanup/1
      , [ fun put_account/1, fun put_transfer/1, fun last_event_count/1]
     }}.


put_account(_) ->
    fun() ->
            Account = #account{account_number = 42, firstname="Reza", surname="Fa", amount = 100 },
            database:put_account(Account),
            ?assertEqual(database:get_account(42), {ok, Account}),
            ?assertEqual(database:get_all_accounts(), [Account])
    end.

put_transfer(_) ->
    fun() ->
            Transfer = #transfer{id = 17, timestamp = {1610,547469,326863}, from_acc_nr = 17, to_acc_nr = 32, amount = 100 },
            database:put_transfer(Transfer),
            ?assertEqual(database:get_transfer(17), {ok, Transfer}),
            ?assertEqual(database:get_all_transfers(), [Transfer]),
            ?assertEqual(database:get_all_transfers(17), [Transfer]),
            ?assertEqual(database:get_all_transfers(16), [])
    end.

last_event_count(_) ->
    fun() ->
        LastAcc1 = database:last_account_service_count(),
        ?assertEqual(0, LastAcc1),
        database:inc_last_account_service_count(),
        LastAcc2 = database:last_account_service_count(),
        ?assertEqual(1, LastAcc2),
        LastTx1 = database:last_transfer_service_count(),
        ?assertEqual(0, LastTx1),
        database:inc_last_transfer_service_count(),
        LastTx2 = database:last_transfer_service_count(),
        ?assertEqual(1, LastTx2)
    end.        



