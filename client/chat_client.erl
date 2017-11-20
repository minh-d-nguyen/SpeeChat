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

print_transcript([]) -> ok;
print_transcript([{Username, Msg, Time} | Rest]) ->
    io:format("~p ~p:~p~n", [Time, Username, Msg]),
    print_transcript(Rest).

%% send_message
%% Take in the username, the PID of the message receiving process, the Server
%% reference, handle input from users apprpriately.
send_message(Username, RecPid, ServNode, Room) ->
    Line = io:get_line("Enter a message: "),
    if
        Line == "--quit\n" ->
            Transcript = gen_server:call({Room, ServNode}, {unsubscribe, Username, RecPid}),
            Sorted = lists:sort(
                fun({UsernameA, MsgA, TimeA}, {UsernameB, MsgB, TimeB}) ->
                    {TimeA, UsernameA, MsgA} =< {TimeB, UsernameB, MsgB} end,
                Transcript
            ),
            io:format("Transcript:~n"),
            print_transcript(Sorted),
            ok;
        Line == "--list\n" ->
            AllSubs = gen_server:call({Room, ServNode}, {subscribers}),
            io:format("Subscribers: ~p~n", [AllSubs]),
            send_message(Username, RecPid, ServNode, Room);
        true ->
            gen_server:cast({Room, ServNode}, {send, Username, Line, timestamp}),
            send_message(Username, RecPid, ServNode, Room)
    end.

%% Exported Client Functions
join_room(ServerNode, Room, Username) ->
    Pid = spawn(chat_client, get_message, []),
    gen_server:call({Room, ServerNode}, {subscribe, Pid, Username}),
    send_message(Username, Pid, ServerNode, Room).
