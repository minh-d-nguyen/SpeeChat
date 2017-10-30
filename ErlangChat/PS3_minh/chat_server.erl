-module(chat_server).
-export([start/1, loop/1]).

start(Name) ->
    Pid = spawn(chat_server, loop, [[]]),
    register(Name, Pid).

loop(CurrentSubs) ->
    receive
        {subscribe, From} -> loop([From | CurrentSubs]);
        {send, Username, Msg} ->
            send_msg_to_all(Username, Msg, CurrentSubs),
            loop(CurrentSubs);
        {stop} -> ok;
        {subscribers, From} ->
            From ! CurrentSubs
    end.

send_msg_to_all(_Username, _Msg, []) ->
    ok;
send_msg_to_all(Username, Msg, [Head | Tail]) ->
    Head ! {message, Username, Msg},
    send_msg_to_all(Username, Msg, Tail).