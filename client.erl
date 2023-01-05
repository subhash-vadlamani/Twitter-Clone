-module(client).
-export[start/0, test/4].

test(UserName, NumTweets, NumSubscribe, false) ->
    io:fwrite("\nEntry Point for Simulator!\n"),

    % connect with the server
    PortNumber = 1204,
    IPAddress = "localhost",
    {ok, Sock} = gen_tcp:connect(IPAddress, PortNumber, [binary, {packet, 0}]),

    % register client
    register_account(Sock, UserName),

    % wait for registration confirmation from the server
    receive
        {tcp, Sock, Data} ->
            io:format("User ~p registered on server", [Data])
    end,

    handle_testing(Sock, UserName, NumTweets, NumSubscribe).

handle_testing(Sock, UserName, NumTweets, NumSubscribe) ->

    % Subscribe
    if
        NumSubscribe > 0 ->
            %change
            SubList = subscriber_generator(1, UserName, NumSubscribe, []),
            subscribe_to_users(Sock, UserName, SubList)
    end,

    % Mention
    UserToMention = rand:uniform(list_to_integer(UserName)),
    send_tweet(Sock, UserName, {"~p mentioning @~p in their tweet",[UserName, UserToMention]}),

    % Hashtag
    send_tweet(Sock, UserName, {"~p says #HashTag in their tweet",[UserName]}).

subscriber_generator(Count, UserName, NumSubscribe, List) ->
    %
    if
        %change
        (Count == UserName) ->
            subscriber_generator(Count+1, UserName, NumSubscribe, List);
        (Count == NumSubscribe) ->
            [count | List];
        true ->
            subscriber_generator(Count+1, UserName, NumSubscribe, [Count | List])
    end.

subscribe_to_users(Sock, UserName, SubList) ->

    [{SubscribeUserName}|RemainingList] = SubList,
    subscribe_to_user(Sock, UserName, SubscribeUserName),
    subscribe_to_users(Sock, UserName, RemainingList).

start() ->
    io:fwrite("\n\n This is the new client\n\n"),
    PortNumber = 1204,
    IPAddress = "localhost",
    {ok, Sock} = gen_tcp:connect(IPAddress, PortNumber, [binary, {packet, 0}]),
    io:fwrite("\n\n Request Sent to server.\n\n"),
    loop(Sock, "_").

loop(Sock, UserName) ->
    receive
        {tcp, Sock, Data} ->
            io:fwrite(Data),
            % ask user for a command
            % user enters a command
            UserName1 = process_user_input(Sock, UserName),
            loop(Sock, UserName1);
        {tcp, closed, Sock} ->
            io:fwrite("Client Cant connect anymore - TCP Closed")
    end.

process_user_input(Sock, UserName) ->

    % ask user for a input command
    {ok, [CommandType]} = io:fread("\nEnter the command: ", "~s\n"),
    io:fwrite(CommandType),

    % parse that command - register, subscribe <user_name>
    if
        CommandType == "register" ->
            % Input user-name
            UserName1 = register_account(Sock);
        CommandType == "tweet" ->
            if
                UserName == "_" ->
                    io:fwrite("Please register first!\n"),
                    UserName1 = process_user_input(Sock, UserName);
                true ->
                    Tweet = io:get_line("\nWhat's on your mind?:"),
                    send_tweet(Sock,UserName, Tweet),
                    UserName1 = UserName
            end;
        CommandType == "retweet" ->
            if
                UserName == "_" ->
                    io:fwrite("Please register first!\n"),
                    UserName1 = process_user_input(Sock, UserName);
                true ->
                    {ok, [Person_UserName]} = io:fread("\nEnter the User Name whose tweet you want to re-post: ", "~s\n"),
                    Tweet = io:get_line("\nEnter the tweet that you want to repost: "),
                    re_tweet(Sock, UserName, Person_UserName, Tweet),
                    UserName1 = UserName
            end;
        CommandType == "subscribe" ->
            if
                UserName == "_" ->
                    io:fwrite("Please register first!\n"),
                    UserName1 = process_user_input(Sock, UserName);
                true ->
                    SubscribeUserName = io:get_line("\nWho do you want to subscribe to?:"),
                    subscribe_to_user(Sock, UserName, SubscribeUserName),
                    UserName1 = UserName
            end;
        CommandType == "query" ->
            if
                UserName == "_" ->
                    io:fwrite("Please register first!\n"),
                    UserName1 = process_user_input(Sock, UserName);
                true ->
                    io:fwrite("\n Querying Options:\n"),
                    io:fwrite("\n 1. My Mentions\n"),
                    io:fwrite("\n 2. Hashtag Search\n"),
                    io:fwrite("\n 3. Subscribed Users Tweets\n"),
                    {ok, [Option]} = io:fread("\nSpecify the task number you want to perform: ", "~s\n"),
                    query_tweet(Sock, UserName, Option),
                    UserName1 = UserName
            end;
        true ->
            io:fwrite("Invalid command!, Please Enter another command!\n"),
            UserName1 = process_user_input(Sock, UserName)
    end,
    UserName1.

register_account(Sock) ->

    % Input user-name
    {ok, [UserName]} = io:fread("\nEnter the User Name: ", "~s\n"),
    % send the server request
    io:format("SELF: ~p\n", [self()]),
    ok = gen_tcp:send(Sock, [["register", ",", UserName, ",", pid_to_list(self())]]),
    io:fwrite("\nAccount has been Registered\n"),
    UserName.

register_account(Sock, UserName) ->
    % send the server request
    io:format("SELF: ~p\n", [self()]),
    ok = gen_tcp:send(Sock, [["register", ",", UserName, ",", pid_to_list(self())]]),
    io:fwrite("\nAccount has been Registered\n"),
    UserName.

send_tweet(Sock,UserName, Tweet) ->
    ok = gen_tcp:send(Sock, ["tweet", "," ,UserName, ",", Tweet]),
    io:fwrite("\nTweet Sent\n"),
    % we call the method to process the user input again so that the client can take more actions.
    process_user_input(Sock, UserName).

re_tweet(Socket, UserName,Person_UserName, Tweet) ->
    ok = gen_tcp:send(Socket, ["retweet", "," ,Person_UserName, ",", UserName,",",Tweet]),
    io:fwrite("\nRetweeted\n"),
    % we call the method to process the user input again so that the client can take more actions.
    process_user_input(Socket, UserName).

subscribe_to_user(Sock, UserName, SubscribeUserName) ->
    ok = gen_tcp:send(Sock, ["subscribe", "," ,UserName, ",", SubscribeUserName]),
    io:fwrite("\nSubscribed!\n"),
    process_user_input(Sock, UserName).

query_tweet(Sock, UserName, Option) ->
    if
        Option == "1" ->
            ok = gen_tcp:send(Sock, ["query", "," ,UserName, ",", "1"]);
        Option == "2" ->
            Hashtag = io:get_line("\nEnter the hahstag you want to search: "),
            ok = gen_tcp:send(Sock, ["query", "," ,UserName, ",","2",",", Hashtag]);
        true ->
            ok = gen_tcp:send(Sock, ["query", "," ,UserName, ",", "3"])
    end,
    io:fwrite("Queried related tweets").
