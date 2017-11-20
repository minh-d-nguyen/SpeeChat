%%%-------------------------------------------------------------------
%%% @author Minh D. Nguyen <mnguye17@vm-hw00.eecs.tufts.edu>
%%% @copyright (C) 2017, Minh D. Nguyen
%%% @doc
%%%
%%% @end
%%% Created : 27 Sep 2017 by Minh D. Nguyen <mnguye17@vm-hw00.eecs.tufts.edu>
%%%-------------------------------------------------------------------
-module(chat_client).

%% Client functions
-export([join_room/3, get_message/0]).

%% The function that will be used in a process to receive and print messages
get_message() ->
    receive
        {message, Username, MessageText} ->
            io:format("~p:~p~n", [Username, MessageText]),
            get_message()
    end.

%% send_message
%% Take in the username, the PID of the message receiving process, the Server
%% reference, handle input from users apprpriately.
send_message(Username, RecPid, ServNode, Room) ->
    {ok, P} = python:start([{python, "python3"}]),
    %%Line = io:get_line("Enter a message: "),
    Line = python:call(P, get_speech, get_speech, []), 
    python:stop(P),
    if
        Line == "--quit\n" ->
            gen_server:cast({Room, ServNode}, {unsubscribe, Username, RecPid}),
            ok;
        Line == "--list\n" ->
            AllSubs = gen_server:call({Room, ServNode}, {subscribers}),
            io:format("Subscribers: ~p~n", [AllSubs]),
            send_message(Username, RecPid, ServNode, Room);
        true ->
            %% Calculate Timestamp in the format YYYY-MM-DD, HH:MM:SS
            %% Time is in UTC
            {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(erlang:timestamp()),
            Timestamp = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w, ~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])),
            gen_server:cast({Room, ServNode}, {send, Username, Line, Timestamp}),
            send_message(Username, RecPid, ServNode, Room)
    end.

%% Exported Client Functions
join_room(ServerNode, Room, Username) ->
    Pid = spawn(chat_client, get_message, []),
    gen_server:call({Room, ServerNode}, {subscribe, Pid, Username}),
    send_message(Username, Pid, ServerNode, Room).
