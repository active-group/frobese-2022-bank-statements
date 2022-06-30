%% This module represents the business logic layer

-module(business_logic).
-include("data.hrl").
-export([
        % previous/reused functions
        get_account/1, sort_tx/1, get_transfers/1 ,
        % new functions
       account_created/4, transfer_created/5
]).

-spec get_account(account_number()) -> {ok, #account{}} | {error, any()}.
get_account(AccountNr) -> database:get_account(AccountNr).

-spec get_transfers(unique_id()) -> list(#transfer{}).
get_transfers(AccountId) ->
     database:get_all_transfers(AccountId).

sort_tx(Txs) ->
  lists:sort(fun(Tx1, Tx2) -> Tx2#transfer.id < Tx1#transfer.id end, Txs).



%% Saves account created events into local DB.
-spec account_created(account_number(),binary(), binary(),money()) ->ok.
account_created(AccNr, Firstname, Surname, InitAmount) ->
    Acc = #account{account_number = AccNr,
                   firstname= Firstname,
                   surname= Surname,
                   amount = InitAmount},
    database:put_account(Acc),
    Acc.


%% Saves transfer created events into local DB. Adjusts account amounts as necessary. No validation whatsoever
-spec transfer_created(unique_id(), account_number(), account_number(), money(),erlang:timestamp()) 
  -> {error, sender_account_not_found | receiver_account_not_found} | {ok}.
transfer_created(TxId, SenderAccountNumber, ReceiverAccountNumber, Amount,  Timestamp) ->
  Transfer = 
      fun() -> 
        MaybeAccSender = database:get_account(SenderAccountNumber),
        MaybeAccReceiver = database:get_account(ReceiverAccountNumber),
        case {MaybeAccSender, MaybeAccReceiver} of
          {{error, not_found}, _} -> {error, sender_account_not_found};
          {_, {error, not_found}} -> {error, receiver_account_not_found};
          
          {{ok, AccSender}, {ok, AccReceiver}} ->
            NewAccSender = AccSender#account{amount = (AccSender#account.amount - Amount)},
            NewAccReceiver = AccReceiver#account{amount = (AccReceiver#account.amount + Amount)},
            Tx = #transfer{id = TxId,
                                timestamp = Timestamp,
                                from_acc_nr = SenderAccountNumber,
                                to_acc_nr = ReceiverAccountNumber,
                                amount = Amount},
            database:put_transfer(Tx),
            database:put_account(NewAccSender),
            database:put_account(NewAccReceiver),
            {ok}
          end
      end,
  database:atomically(Transfer).



