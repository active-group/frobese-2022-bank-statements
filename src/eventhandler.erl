-module(eventhandler).
-export([init/1, handle_cast/2,
         start/0]).

-behaviour(gen_server).



init(_) ->
    {ok, ok}. % Start ist der initiale Wert für N

start() ->
    gen_server:start(
                    {local, bankStatementService},
                    ?MODULE, []
                    ,
                    [] 
                    % [{debug,[error]}]         % beispiel für Debug-Funktion
                ).


handle_cast({account_service, 
            count = Count, 
            create_account,
            {account,  % <--record name
                            account_number=AccNr, name=Name, surname=Surname, amount=Amount}  
        } , _) ->
    ExpectedCount = database:last_transfer_service_count() + 1,
    if 
        ExpectedCount == Count ->
                business_logic:account_created(AccNr, Name,Surname,Amount),
                database:inc_last_account_service_count();
        true -> true % else-zweig --> TODO: register um die fehlendne zu bekommen
    end,
    {noreply,[]};
handle_cast({transfer_service, 
                count=Count, 
                {transferEvent, % <--record name
                    id=Id ,from_acc_nr=FromAccNr, to_acc_nr=ToAccNr, amount=Amount }} , _) ->
    ExpectedCount = database:last_transfer_service_count() + 1,
    if 
        ExpectedCount == Count ->
                business_logic:transfer_created( Id, FromAccNr,ToAccNr, Amount),
                database:inc_last_transfer_service_count();
        true -> true % else-zweig --> TODO: register um die fehlendne zu bekommen
        end,
    {noreply,[]}.

