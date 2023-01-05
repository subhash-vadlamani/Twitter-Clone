-module(simulator).
-export[start/0].

start() ->
    io:fwrite("\n\n Simulator Running\n\n"),
    
    {ok, [Input_NumClients]} = io:fread("\nNumber of clients to simulate: ", "~s\n"),
    {ok, [Input_MaxSubscribers]} = io:fread("\nMaximum Number of Subscribers a client can have: ", "~s\n"),
    {ok, [Input_DisconnectClients]} = io:fread("\nclient disconnection percentage ", "~s\n"),

    % converting the inputs to integers from string
    NumClients = list_to_integer(Input_NumClients),
    MaxSubscribers = list_to_integer(Input_MaxSubscribers),
    DisconnectClients = list_to_integer(Input_DisconnectClients),
    % getting the number of clients to disconnect
    ClientsToDisconnect = DisconnectClients * (0.01) * NumClients,

    Main_Table = ets:new(messages, [ordered_set, named_table, public]),
    % Method to create the clients one by one
    clientCreationProcess(1, NumClients, MaxSubscribers, Main_Table),

    %Clients = clientCreationProcess(NumClients),
    
    %start time
    Start_Time = erlang:system_time(millisecond),
    %checkAliveClients(Clients),
    %End time
    End_Time = erlang:system_time(millisecond),
    io:format("\nTime Taken to Converge: ~p milliseconds\n", [End_Time - Start_Time]).


% Function to spawn a client - and figure out its properties (UserName, NumTweets, NumSubscribe, PID)
clientCreationProcess(Count, NumClients, MaxSubcribers, Main_Table) ->
    UserName = Count,
    NumTweets = round(floor(MaxSubcribers/Count)),
    NumSubscribe = round(floor(MaxSubcribers/(NumClients-Count+1))) - 1,

    PID = spawn(client, test, [UserName, NumTweets, NumSubscribe, false]),

    ets:insert(Main_Table, {UserName, PID}),
    % We continue the client creation process till the given number of clients have been created.
    if 
        Count == NumClients ->
            ok;
        true ->
            clientCreationProcess(Count+1, NumClients, MaxSubcribers, Main_Table)
    end.

