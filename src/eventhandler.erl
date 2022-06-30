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
    gen_server:cast(AccountsNode, {register, self(), database:last_account_service_count()}),
    gen_server:cast(TransferNode, {register, self(), database:last_transfer_service_count()})
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
            count = Count, 
            create_account,
            {account,  % <--record name
                            account_number=AccNr, name=Name, surname=Surname, amount=Amount}  
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
                count=Count, 
                {transferEvent, % <--record name
                    id=Id ,from_acc_nr=FromAccNr, to_acc_nr=ToAccNr, amount=Amount }} , Nodes) ->
    ExpectedCount = database:last_transfer_service_count() + 1,
    if 
        ExpectedCount == Count ->
                business_logic:transfer_created( Id, FromAccNr,ToAccNr, Amount),
                database:inc_last_transfer_service_count();
        true -> reRegister(Nodes) % else-zweig --> register um die fehlendne zu bekommen
    end,
    {noreply,Nodes}.

