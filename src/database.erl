%% This module represents the database layer

-module(database).
-include("data.hrl").
-export([init_database/0, write/2, read_all/2,
         put_account/1, get_account/1, get_all_accounts/0,
         put_transfer/1, get_transfer/1, get_all_transfers/0, get_all_transfers/1, 
         unique_account_number/0,unique_tx_id/0, 
         atomically/1
        ,last_account_service_count/0
        ,last_transfer_service_count/0
        ,inc_last_account_service_count/0
        ,inc_last_transfer_service_count/0
    ]).

close_tables() ->
    dets:close(transfer),
    dets:close(account),
    dets:close(event),
    dets:close(last_event_count),
    dets:close(table_id).

%% destroy tables in case they already existed
destroy_tables() ->
    file:delete("transfer.dets"),
    file:delete("account.dets"),
    file:delete("event.dets"),
    file:delete("last_event_count.dets"),
    file:delete("table_id.dets").

% unfortunately, delete_table doesn't always work such that create_table doesn't fail, so don't check return value
create_tables() ->
    {ok, transfer} = dets:open_file(transfer, [{type, set}, {file, "transfer.dets"}]),
    {ok, account} = dets:open_file(account, [{type, set}, {file, "account.dets"}]),
    {ok, event} = dets:open_file(event, [{type, set}, {file, "event.dets"}]),
    {ok, last_event_count} = dets:open_file(last_event_count, [{type, set}, {file, "last_event_count.dets"}]),
    {ok, table_id} = dets:open_file(table_id, [{type, set}, {file, "table_id.dets"}]),
    dets:insert(table_id, {transfer, 0}),
    dets:insert(table_id, {account, 0}),
    dets:insert(table_id, {event, 0}),
    dets:insert(last_event_count, {account_servicec, 0}),
    dets:insert(last_event_count, {transfer_service, 0}).

init_database() ->
    close_tables(),
    destroy_tables(),
    create_tables(),
    ok.

write(Table, Tuple) ->
    ok = dets:insert(Table, Tuple),
    ok.

-spec read_one(dets:tab_name(), unique_id(), fun((tuple()) -> Obj)) -> {ok, Obj} | {error, not_found | more_than_one}.
read_one(Table, Id, Deserialize) ->
    Res = dets:lookup(Table, Id),
    case Res of
        [Tuple] -> {ok, Deserialize(Tuple)};
        []  -> {error, not_found};
        [_ | _] -> {error, more_than_one};
        Error -> Error
    end.

-spec read_all(dets:tab_name(), fun((tuple()) -> Obj)) -> list(Obj).
read_all(Table, Deserialize) ->
    Res = dets:select(Table,[{'_',[],['$_']}]),    
    lists:map(Deserialize, Res).

-spec put_account(#account{}) -> ok.
put_account(#account{account_number = AccountNumber,  firstname= Firstname, surname= Surname, amount = Amount}) ->
    write(account, {AccountNumber, Firstname, Surname, Amount}).

deserialize_account({AccountNumber,  Firstname, Surname, Amount}) ->
    #account{account_number = AccountNumber,firstname= Firstname, surname= Surname, amount = Amount}.

-spec get_account(account_number()) -> {ok, #account{}} | {error, any()}.
get_account(AccountNumber) ->
    read_one(account, AccountNumber, fun deserialize_account/1).

-spec get_all_accounts() -> list(#account{}).
get_all_accounts() -> read_all(account, fun deserialize_account/1).


-spec put_transfer(#transfer{}) -> ok.
put_transfer(#transfer{id = Id, timestamp = Timestamp, from_acc_nr = FromAccNr, to_acc_nr = ToAccNr, amount = Amount}) ->
    write(transfer, {Id, Timestamp, FromAccNr, ToAccNr, Amount}).

deserialize_transfer({Id, Timestamp, FromAccNr, ToAccNr, Amount}) ->
    #transfer{id = Id, timestamp = Timestamp, from_acc_nr = FromAccNr, to_acc_nr = ToAccNr, amount = Amount}.

-spec get_transfer(unique_id()) -> {ok, #transfer{}} |??{error, any()}.
get_transfer(Id) ->
    read_one(transfer, Id, fun deserialize_transfer/1).

-spec get_all_transfers() -> list(#transfer{}).
get_all_transfers() -> read_all(transfer, fun deserialize_transfer/1).

-spec get_all_transfers(account_number()) -> list(#transfer{}).
get_all_transfers(AccountNr) ->
    Res = dets:select(transfer,
                           [{'$1',
                            [{'orelse',
                                {'==', {element, 3, '$1'}, AccountNr},
                                {'==', {element, 4, '$1'}, AccountNr}}],
                            ['$_']}]),
    lists:map(fun deserialize_transfer/1, Res).

-spec unique_account_number() -> unique_id().
unique_account_number() -> dets:update_counter(table_id, account, 1).


-spec unique_tx_id() -> unique_id().
unique_tx_id() -> dets:update_counter(table_id, transfer, 1).


% ----- get last known count 
 
-spec last_account_service_count() -> integer().
last_account_service_count() ->
    case read_one(last_event_count, account_service, fun deserialize_last_event_count_int_only/1 ) of
        {error, not_found} -> 0;
        {ok, Count} -> Count
    end.
-spec last_transfer_service_count() -> integer().
last_transfer_service_count() ->
    case read_one(last_event_count, transfer_service, fun deserialize_last_event_count_int_only/1 ) of
        {error, not_found} -> 0;
        {ok, Count} -> Count
    end.

deserialize_last_event_count_int_only({_,Count}) ->
    Count.


-spec inc_last_account_service_count() -> ok.
inc_last_account_service_count() -> write(last_event_count, {account_service, last_account_service_count() + 1 }).
-spec inc_last_transfer_service_count() -> ok.
inc_last_transfer_service_count() -> write(last_event_count, {transfer_service, last_transfer_service_count() + 1 }).

% holdover from Mnesia
-spec atomically(fun(() -> Ret)) -> Ret.
atomically(Fun) ->
    Fun().
