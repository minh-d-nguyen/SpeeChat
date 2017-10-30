-module(chat_client).
-export([join_room/3, join/3, get_message/0]).

join_room(ServerNode, Room, Username) ->
    Pid = spawn(chat_client, get_message, []),
    {ServerNode, Room} ! {subscribe, Pid},
    send_message(Username, ServerNode, Room).

get_message() ->
    receive
        {message, Username, MessageText} ->
            io:format("~p:~p~n", [Username, MessageText]),
            get_message()
    end.

send_message(Username, ServerNode, ServerName) ->
    Line = io:get_line("Enter a message: "),
    {ServerName, ServerNode} ! {send, Username, Line},
    send_message(Username, ServerNode, ServerName).
