%% This module represents the business logic layer

-module(business_logic).
-include("data.hrl").
-export([
        % previous/reused functions
        get_account/1, get_person/1, sort_tx/1, get_transfers/1 ,
        % new functions
          person_created/3, account_created/3, transfer_created/5
]).

-spec get_person(unique_id()) -> {ok, #person{} | {error, any()}}.
get_person(Id) -> database:get_person(Id).

-spec get_account(account_number()) -> {ok, #account{}} | {error, any()}.
get_account(AccountNr) -> database:get_account(AccountNr).

-spec get_transfers(unique_id()) -> list(#transfer{}).
get_transfers(Id) ->
     database:get_all_transfers(Id).

sort_tx(Txs) ->
  lists:sort(fun(Tx1, Tx2) -> Tx2#transfer.id < Tx1#transfer.id end, Txs).


%% Saves person created events into local DB. 
-spec person_created(unique_id(), binary(),binary()) -> #person{}.
person_created(PersId,Firstname, Surname) ->
    Pers = #person{id = PersId,
                   firstname = Firstname,
                   surname = Surname},
    database:put_person(Pers),
    Pers.

%% Saves account created events into local DB.
-spec account_created(account_number(),unique_id(),money()) ->ok.
account_created(AccNr, PersId, InitAmount) ->
    Acc = #account{account_number = AccNr,
                   person_id = PersId,
                   amount = InitAmount},
    database:put_account(Acc),
    Acc.

%% Saves transfer created events into local DB. Adjusts account amounts as necessary. No validation whatsoever
-spec transfer_created(account_number(), account_number(), money(),unique_id(), timeout()) -> ok.
transfer_created(SenderAccountNumber, ReceiverAccountNumber, Amount, TxId, Timestamp) ->
  Transfer = 
      fun() -> 
        AccSender = database:get_account(SenderAccountNumber),
        AccReceiver = database:get_account(ReceiverAccountNumber),
        NewAccSender = AccSender#account{amount = (AccSender#account.amount - Amount)},
        NewAccReceiver = AccReceiver#account{amount = (AccReceiver#account.amount + Amount)},
        Tx = #transfer{id = TxId,
                            timestamp = Timestamp,
                            from_acc_nr = SenderAccountNumber,
                            to_acc_nr = ReceiverAccountNumber,
                            amount = Amount},
        database:put_transfer(Tx),
        database:put_account(NewAccSender),
        database:put_account(NewAccReceiver)
      end,
  database:atomically(Transfer).



