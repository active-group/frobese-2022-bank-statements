-module(eventhandler).
-export([init/1, handle_cast/2,handle_info/2,
         start/2]).

-behaviour(gen_server).



init(Nodes) ->
    io:format("Nodes: ~w~n", [Nodes]),
    reRegister(Nodes),
    timer:send_interval(10000, reRegister),
    {ok, Nodes}. % state is in dets, no real state here

reRegister({AccountsNode, TransferNode}) ->
    io:format("reRegister at: ~w, ~w ~n", [AccountsNode, TransferNode]),
    gen_server:cast({account_feed, AccountsNode}, {register, self(), account_service, database:last_account_service_count()}),
    gen_server:cast({transfer_feed, TransferNode}, {register, self(), transfer_service, database:last_transfer_service_count()})
    .



start(AccountsNode, TransferNode) ->
    gen_server:start(
                    {local, bankStatementService},
                    ?MODULE, {AccountsNode, TransferNode}
                    ,
                    [] 
                    % [{debug,[error]}]         % beispiel für Debug-Funktion
                ).

handle_info(reRegister, Nodes) ->
    reRegister(Nodes),
    {noreply, Nodes}.

handle_cast({account_service, 
            Count, 
            create_account,
            {account,  % <--record name
                            AccNr, Name, Surname, Amount}  
        } , Nodes) ->
    ExpectedCount = database:last_account_service_count() + 1,
    if 
        ExpectedCount == Count ->
                business_logic:account_created(AccNr, Name,Surname,Amount),
                database:inc_last_account_service_count();
        true -> reRegister(Nodes) % else-zweig --> register um die fehlendne zu bekommen
    end,
    {noreply,Nodes};
handle_cast({transfer_service, 
                Count, 
                {transferEvent, % <--record name
                    Id ,FromAccNr, ToAccNr, Amount }} , Nodes) ->
    ExpectedCount = database:last_transfer_service_count() + 1,
    if 
        ExpectedCount == Count ->
                business_logic:transfer_created( Id, FromAccNr,ToAccNr, Amount),
                database:inc_last_transfer_service_count();
        true -> reRegister(Nodes) % else-zweig --> register um die fehlendne zu bekommen
    end,
    {noreply,Nodes}.

