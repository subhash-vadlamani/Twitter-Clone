-module(twitter_engine).
-import(maps, []).
-export[start/0].

start() ->
    io:fwrite("\n\n Twitter Engine Clone is now running! \n\n"),
    Table = ets:new(messages, [ordered_set, named_table, public]),
    Client_Socket_Mapping = ets:new(clients, [ordered_set, named_table, public]),
    All_Clients = [],
    Map = maps:new(),
    {ok, ListenSocket} = gen_tcp:listen(1204, [binary, {keepalive, true}, {reuseaddr, true}, {active, false}]),
    await_connections(ListenSocket, Table, Client_Socket_Mapping).

await_connections(Listen, Table, Client_Socket_Mapping) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    ok = gen_tcp:send(Socket, "User"),
    spawn(fun() -> await_connections(Listen, Table, Client_Socket_Mapping) end),
    % Initially we send the initial user details as an empty list.
    do_recv(Socket, Table, [], Client_Socket_Mapping).


do_recv(Socket, Table, InitialUser, Client_Socket_Mapping) ->


    io:fwrite("Receive Now \n\n"),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data1} ->
            
            Data = re:split(Data1, ","),
            Type = binary_to_list(lists:nth(1, Data)),

            io:format("\n\nDATA: ~p\n\n ", [Data]),
            io:format("\n\nTYPE: ~p\n\n ", [Type]),

            if 
                Type == "register" ->
                    UserName = binary_to_list(lists:nth(2, Data)),
                    % This is the username that we want to register the user with %
                    PID = binary_to_list(lists:nth(3, Data)),
                    % This is the process ID that called the method to register a new user
                    io:format("\nPID:~p\n", [PID]),
                    io:format("\nSocket:~p\n", [Socket]),
                    io:format("Type: ~p\n", [Type]),
                    io:format("\n~p wants to register an account\n", [UserName]),
                    
                    Output = ets:lookup(Table, UserName),
                    io:format("Output: ~p\n", [Output]),
                    if
                        Output == [] ->

                            % If the user with the above username does not exist, we are going
                            % to create an entry in the table with that username and
                            % initilize the variable which hold the followers and tweet details

                            ets:insert(Table, {UserName, [{"followers", []}, {"tweets", []}]}),      
                            ets:insert(Client_Socket_Mapping, {UserName, Socket}),                
                            UserList = ets:lookup(Table, UserName),
                            io:format("~p", [lists:nth(1, UserList)]),
                            % Here, we print the username of the newly created user.


                          
                            ok = gen_tcp:send(Socket, "User has been registered"), % RESPOND BACK - YES/NO
                            io:fwrite("User creation possible, Key is not in database\n");
                        true ->
                            ok = gen_tcp:send(Socket, "Username is already in use. Please try a different username"),
                            io:fwrite("Duplicate key!\n")
                    end,
                    do_recv(Socket, Table, [UserName], Client_Socket_Mapping);

                Type == "tweet" ->
                    UserName = binary_to_list(lists:nth(2, Data)),
                    Tweet = binary_to_list(lists:nth(3, Data)),
                    io:format("\n ~p sent the following tweet: ~p", [UserName, Tweet]),
                    
                    % {ok, Val} = maps:find(UserName, Map),
                    Val = ets:lookup(Table, UserName),
                    io:format("Output: ~p\n", [Val]),
                    Val3 = lists:nth(1, Val),
                    UserMapping = element(2, Val3),
                    UserDetails = maps:from_list(UserMapping),
                    {ok, CurrentFollowers} = maps:find("followers",UserDetails),
                    {ok, CurrentTweets} = maps:find("tweets",UserDetails),

                    NewTweets = CurrentTweets ++ [Tweet],
                    io:format("~p~n",[NewTweets]),
                    
                    ets:insert(Table, {UserName, [{"followers", CurrentFollowers}, {"tweets", NewTweets}]}),
                  
                    sendMessage(Socket, Client_Socket_Mapping, Tweet, CurrentFollowers, UserName),
                    do_recv(Socket, Table, [UserName], Client_Socket_Mapping);

                Type == "retweet" ->
                    Person_UserName = binary_to_list(lists:nth(2, Data)), % username of the current person  %
                    UserName = binary_to_list(lists:nth(3, Data)), % username of the person who we want to retweet %
                    Sub_User = string:strip(Person_UserName, right, $\n),
                    io:format("User to retweet from: ~p\n", [Sub_User]),
                    Tweet = binary_to_list(lists:nth(4, Data)),
                    Out = ets:lookup(Table, Sub_User),
                    if
                        Out == [] ->
                            % No user with the username 'Person_UserName' exists in the system%
                            io:fwrite("User does not exist!\n");
                        true ->
                            % Current User
                            Out1 = ets:lookup(Table, UserName),
                            Val3 = lists:nth(1, Out1),
                            UserMapping = element(2, Val3),
                            UserDetails = maps:from_list(UserMapping),
                            % User we are retweeting from
                            Val_3 = lists:nth(1, Out),
                            OriginalUserMapping = element(2, Val_3),
                            OriginalTweetUserDetails = maps:from_list(OriginalUserMapping),
                            % current user
                            {ok, CurrentFollowers} = maps:find("followers",UserDetails),
                            % user we are retweeting from
                            {ok, CurrentTweets} = maps:find("tweets",OriginalTweetUserDetails),
                            io:format("Tweet to be re-posted: ~p\n", [Tweet]),
                            CheckTweet = lists:member(Tweet, CurrentTweets),
                            if
                                CheckTweet == true ->
                                    NewTweet = string:concat(string:concat(string:concat("re:",Sub_User),"->"),Tweet),
                                    sendMessage(Socket, Client_Socket_Mapping, NewTweet, CurrentFollowers, UserName);
                                true ->
                                    io:fwrite("Tweet does not exist!\n")
                            end     
                    end,
                    io:format("\n ~p wants to retweet something", [UserName]),
                    do_recv(Socket, Table, [UserName], Client_Socket_Mapping);

                Type == "subscribe" ->
                    UserName = binary_to_list(lists:nth(2, Data)),
                    SubscribedUserName = binary_to_list(lists:nth(3, Data)),
                    Sub_User = string:strip(SubscribedUserName, right, $\n),

                    Output1 = ets:lookup(Table, Sub_User),
                    io:format("Output: ~p\n", [Output1]),

                    if
                        Output1 == [] ->
                            io:fwrite("No user with the username ~p exists, Please try a different user name \n", [Sub_User]);
                        true ->

                            Val = ets:lookup(Table, Sub_User),
                            io:format("~p~n",[Val]),
                            Val3 = lists:nth(1, Val),
                            UserMapping = element(2, Val3),

                            UserDetails = maps:from_list(UserMapping),
                            {ok, CurrentFollowers} = maps:find("followers",UserDetails),
                            {ok, CurrentTweets} = maps:find("tweets",UserDetails),

                            NewFollowers = CurrentFollowers ++ [UserName],
                            io:format("~p~n",[NewFollowers]),
                        
                            ets:insert(Table, {Sub_User, [{"followers", NewFollowers}, {"tweets", CurrentTweets}]}),

                            ok = gen_tcp:send(Socket, "Subscribed!"),

                            do_recv(Socket, Table, [UserName], Client_Socket_Mapping)
                    end,
                    io:format("\n ~p wants to subscribe to ~p\n", [UserName, Sub_User]),
                    ok = gen_tcp:send(Socket, "Subscribed!"),
                    do_recv(Socket, Table, [UserName], Client_Socket_Mapping);

                Type == "query" ->
                    Option = binary_to_list(lists:nth(3, Data)),
                    UserName = binary_to_list(lists:nth(2, Data)),
                    % Query = binary_to_list(lists:nth(3, Data)),
                    if
                        Option == "1" ->
                            io:fwrite("My mentions!\n");
                        Option == "2" ->
                            io:fwrite("Hashtag Search\n"),
                            Hashtag = binary_to_list(lists:nth(4, Data)),
                            io:format("Hashtag: ~p\n", [Hashtag]);
                        true ->
                            io:fwrite("Subscribed User Search\n"),
                            % Sub_UserName = binary_to_list(lists:nth(4, Data)),
                            Sub_UserName = ets:first(Table),
                            Sub_User = string:strip(Sub_UserName, right, $\n),
                            io:format("Sub_UserName: ~p\n", [Sub_User]),
                            Val = ets:lookup(Table, Sub_User),
                            % io:format("~p~n",[Val]),
                            Val3 = lists:nth(1, Val),
                            UserMapping = element(2, Val3),
                            UserDetails = maps:from_list(UserMapping),
                            {ok, CurrentTweets} = maps:find("tweets",UserDetails),
                            io:format("\n ~p : ", [Sub_User]),
                            io:format("~p~n",[CurrentTweets]),
                            searchWholeTable(Table, Sub_User, UserName)
                    end,
                    io:format("\n ~p wants to query", [UserName]),
                    do_recv(Socket, Table, [UserName], Client_Socket_Mapping);
                true ->
                    io:fwrite("\n Anything else!")
            end;

        {error, closed} ->
            {ok, list_to_binary(InitialUser)};
        {error, Reason} ->
            io:fwrite("error"),
            io:fwrite(Reason)
    end.


searchWholeTable(Table, Key, UserName) ->
    CurrentRow_Key = ets:next(Table, Key),
    Val = ets:lookup(Table, CurrentRow_Key),
    % io:format("~p~n",[Val]),
    Val3 = lists:nth(1, Val),
    UserMapping = element(2, Val3),
    UserDetails = maps:from_list(UserMapping),
    {ok, CurrentFollowers} = maps:find("followers",UserDetails),
    IsMember = lists:member(UserName, CurrentFollowers),
    if
        IsMember == true ->
            {ok, CurrentTweets} = maps:find("tweets",UserDetails),
            io:format("\n ~p : ", [CurrentRow_Key]),
            io:format("~p~n",[CurrentTweets]),
            searchWholeTable(Table, CurrentRow_Key, UserName);
        true ->
            io:fwrite("\n No more tweets!\n")
    end,
    io:fwrite("\n Searching the whole table!\n").


sendMessage(Socket, Client_Socket_Mapping, Tweet, Subscribers, UserName) ->
    if
        Subscribers == [] ->
            io:fwrite("\nNo followers!\n");
        true ->

            [Client_To_Send | Remaining_List ] = Subscribers,
            % Here, we seperate the list of subscribers to one client and the rest of the client
            % After that, we we send the tweet to that one client and
            % then recursively call the function to send message to the remaining users
            io:format("Client to send: ~p\n", [Client_To_Send]),
            io:format("\nRemaining List: ~p~n",[Remaining_List]),
            Client_Socket_Row = ets:lookup(Client_Socket_Mapping,Client_To_Send),
            Val3 = lists:nth(1, Client_Socket_Row),
            Client_Socket = element(2, Val3),
            io:format("\nClient Socket: ~p~n",[Client_Socket]),
            
            ok = gen_tcp:send(Client_Socket, ["New tweet received!\n",",",UserName,":",Tweet]),
            ok = gen_tcp:send(Socket, "Your tweet has been sent"),
            
            sendMessage(Socket, Client_Socket_Mapping, Tweet, Remaining_List, UserName)
    end,
    io:fwrite("Send message!\n").

