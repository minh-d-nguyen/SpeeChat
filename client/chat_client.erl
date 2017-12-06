%%%-------------------------------------------------------------------
%%% @author Minh D. Nguyen, Quinn Collins, and Arpan Gurung
%%% @doc Erlang module for client side chat application
%%%-------------------------------------------------------------------
-module(chat_client).
-behavior(gen_server).

%% Client functions
-export([join_room/2, get_speech/0, send_message/5, get_message/0, start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-define(SERVER, ?MODULE).

%% The function that will be used in a process to receive and print messages
get_message() ->
    receive
        {message, Username, MessageText, Time} ->
            gen_server:call(?SERVER, {msg, {Username, MessageText, Time}}),
            get_message();
        {stop} -> ok
    end.

print_transcript([]) -> ok;
print_transcript([{Username, Msg, Time} | Rest]) ->
    io:format("~p ~p:~p~n", [Time, Username, Msg]),
    print_transcript(Rest).

%% send_message
%% Take in the username, the PID of the message receiving process, the Server
%% reference, handle input from users apprpriately.
send_message(Username, RecPid, SpeechPid, Room, PythonPID) ->
    receive
        {newmsg, Msg} ->
            Line = Msg,
            if
                Line == <<"--quit">> ->
                    Transcript = gen_server:call(
                        {global, Room}, {unsubscribe, Username, RecPid}
                    ),
                    Sorted = lists:sort(
                        fun({UsernameA, MsgA, TimeA}, {UsernameB, MsgB, TimeB}) ->
                            {TimeA, UsernameA, MsgA} =< {TimeB, UsernameB, MsgB} end,
                        Transcript
                    ),
                    io:format("Transcript:~n"),
                    print_transcript(Sorted),
                    gen_server:stop(?SERVER),
                    python:stop(PythonPID),
                    RecPid ! {stop},
                    SpeechPid ! {stop},
                    ok;
                Line == <<"--list">> ->
                    AllSubs = gen_server:call({global, Room}, {subscribers}),
                    io:format("Subscribers: ~p~n", [AllSubs]),
                    send_message(Username, RecPid, SpeechPid, Room, PythonPID);
                true ->
                    %% Calculate Timestamp in the format YYYY-MM-DD, HH:MM:SS
                    %% Time is in UTC
                    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(
                        erlang:timestamp()
                    ),
                    Timestamp = lists:flatten(
                        io_lib:format(
                            "~4..0w-~2..0w-~2..0w, ~2..0w:~2..0w:~2..0w",
                            [Year,Month,Day,Hour,Minute,Second]
                        )
                    ),
                    gen_server:cast({global, Room}, {send, Username, Line, Timestamp}),
                    send_message(Username, RecPid, SpeechPid, Room, PythonPID)
            end
    end.

get_speech() ->
    {ok, P} = python:start([{python, "python"}]),
    Line = python:call(P, get_speech, get_speech, []),
    io:format("Got: ~p~n", [Line]),
    python:stop(P),
    if
        Line == <<>> ->
            receive
                {stop} -> ok
            after 10 ->
                get_speech()
            end;
        true ->
            io:format("Sending: ~p~n", [Line]),
            NewLine = string:concat("*", Line),
            gen_server:call(?SERVER, {speech, {NewLine}}),
            receive
                {stop} -> ok
            after 10 ->
                get_speech()
            end
    end.

%% Exported Client Functions
join_room(Room, Username) ->
    {ok, PythonPID} = python:start([{python, "python"}]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    RecPid = spawn(chat_client, get_message, []),
    Transcript = gen_server:call({global, Room}, {subscribe, Username, RecPid}),
    SortedTranscript = lists:map(
        fun({Name, Msg, Time}) ->
            list_to_binary(
                string:join(
                    [string:join([Time, atom_to_list(Name)], " "), Msg],
                    ": "
                )
            )
        end, Transcript),
    SpeechPid = spawn(chat_client, get_speech, []),
    SendPid = spawn(chat_client, send_message, [Username, RecPid, SpeechPid, Room, PythonPID]),
    python:call(PythonPID, speechat_gui, create_gui, [Username, SendPid, SortedTranscript]).

% Gen_server behavior
init(_Args) ->
    {ok, Context} = erlzmq:context(),
    {ok, Publisher} = erlzmq:socket(Context, [push, {active, false}]),
    ok = erlzmq:bind(Publisher, "tcp://127.0.0.1:5561"),
    {ok, Publisher}.

handle_call({speech, {Line}}, _From, Socket) ->
    erlzmq:send(Socket, list_to_binary(Line)),
    {reply, ok, Socket};

handle_call({msg, {Name, Msg, Time}}, _From, Socket) ->
    StrMsg = string:join([string:join([Time, atom_to_list(Name)], " "), Msg], ": "),
    erlzmq:send(Socket, list_to_binary(StrMsg)),
    {reply, ok, Socket}.

handle_cast(_A, _B) ->
    ok.

terminate(_A, Socket) ->
    erlzmq:close(Socket),
    ok.

stop() ->
    gen_server:stop({local, ?SERVER}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
