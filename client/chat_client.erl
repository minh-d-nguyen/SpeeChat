%%%-------------------------------------------------------------------
%%% @chat_client.erl
%%% @author Minh D. Nguyen, Quinn Collins, and Arpan Gurung
%%% @doc Erlang module for client side of chat application SpeeChat
%%% NOTE: The erlzmq communication code is adapted from the example
%%% at the website:
%%% http://www.23min.com/2013/03/erlang-to-pyqt-via-0mq/
%%%-------------------------------------------------------------------
-module(chat_client).
-behavior(gen_server).

%% Client functions
-export([join_room/2, get_speech/0, send_message/5, get_message/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
%% unused functions, exported to prevent warnings 
-export([start_link/0, handle_info/2, code_change/3]).
-define(SERVER, ?MODULE).

%% receive messages from the server
get_message() ->
    receive
        {message, Username, MessageText, Time} ->
            gen_server:call(?SERVER, {msg, {Username, MessageText, Time}}),
            get_message();
        {stop} -> ok
    end.

%% print the transcript of the conversation. Each message includes the
%% timestamp, username, and message text
print_transcript([]) -> ok;
print_transcript([{Username, Msg, Time} | Rest]) ->
    io:format("~p ~p:~p~n", [Time, Username, Msg]),
    print_transcript(Rest).

%% Take in the username, the PID of the message receiving process, the Server
%% reference, handle input from users apprpriately.
send_message(Username, RecPid, SpeechPid, Room, PythonPID) ->
    receive
        {newmsg, Msg} ->
            Line = Msg,
            if
                Line == <<"--quit">> ->
                    %% unsubscribe from the room, print the conversation 
                    %% transcript, and send stop signal to processes for speech
                    %% and text input
                    Transcript = gen_server:call(
                        {global, Room}, {unsubscribe, Username, RecPid}
                    ),
                    Sorted = lists:sort(
                        fun({UsernameA, MsgA, TimeA}, 
                            {UsernameB, MsgB, TimeB}) ->
                            {TimeA, UsernameA, MsgA} =< 
                            {TimeB, UsernameB, MsgB} end,
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
                    %% List subscribers of the room
                    AllSubs = gen_server:call({global, Room}, {subscribers}),
                    io:format("Subscribers: ~p~n", [AllSubs]),
                    send_message(Username, RecPid, SpeechPid, Room, PythonPID);
                true ->
                    %% Send message to server to be distributed. Calculate the
                    %% message timestamp, in format YYYY-MM-DD, HH:MM:SS UTC,
                    %% and send it with the username and text of the message
                    {{Year, Month, Day}, {Hour, Minute, Second}} = 
                                                    calendar:now_to_datetime(
                        erlang:timestamp()
                    ),
                    Timestamp = lists:flatten(
                        io_lib:format(
                            "~4..0w-~2..0w-~2..0w, ~2..0w:~2..0w:~2..0w",
                            [Year,Month,Day,Hour,Minute,Second]
                        )
                    ),
                    gen_server:cast({global, Room}, 
                                    {send, Username, Line, Timestamp}),
                    send_message(Username, RecPid, SpeechPid, Room, PythonPID)
            end
    end.

%% Start a python process to handle speech input. 
get_speech() ->
    {ok, P} = python:start([{python, "python"}]),
    Line = python:call(P, get_speech, get_speech, []),
    python:stop(P),
    if
        Line == <<>> ->
            % check if stop signal is received
            receive
                {stop} -> ok
            after 10 ->
                get_speech()
            end;
        true ->
            %% add * to indicate text is from speech
            NewLine = string:concat("*", Line),
            gen_server:call(?SERVER, {speech, {NewLine}}),
            % check if stop signal is received
            receive
                {stop} -> ok
            after 10 ->
                get_speech()
            end
    end.

%% join chat room with specified username. User must have previously called
%% net_adm:ping/1 on the node they want to connect to, and received the
%% response "pong".
%% Room must match the name of a running room, and username must be a string.
join_room(Room, Username) ->
    %% start python process for GUI
    {ok, PythonPID} = python:start([{python, "python"}]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    %% start erlang process to receive messages
    RecPid = spawn(chat_client, get_message, []),
    %% get transcript of previous messages and display them in order
    Transcript = gen_server:call({global, Room}, 
                                 {subscribe, Username, RecPid}),
    SortedTranscript = lists:map(
        fun({Name, Msg, Time}) ->
            list_to_binary(
                string:join(
                    [string:join([Time, Name], " "), Msg],
                    ": "
                )
            )
        end, Transcript),
    %% spawn erlang process to receive messages from speech-to-text
    SpeechPid = spawn(chat_client, get_speech, []),
    %% spawn erlang process to send messages to the server
    SendPid = spawn(chat_client, send_message, 
                    [Username, RecPid, SpeechPid, Room, PythonPID]),
    %% create GUI in python process
    python:call(PythonPID, speechat_gui, create_gui, 
                [Username, SendPid, SortedTranscript]).

%% ============================================================================
%% gen_server behavior functions for erlzmq communication
%% NOTE: The erlzmq communication code is adapted from the example
%% at the website:
%% http://www.23min.com/2013/03/erlang-to-pyqt-via-0mq/
%% ============================================================================

%% initialize and open port for erlzmq for communication between GUI and client
init(_Args) ->
    {ok, Context} = erlzmq:context(),
    {ok, Publisher} = erlzmq:socket(Context, [push, {active, false}]),
    ok = erlzmq:bind(Publisher, "tcp://127.0.0.1:5561"),
    {ok, Publisher}.

%% handle speech-to-text sent to socket from GUI
handle_call({speech, {Line}}, _From, Socket) ->
    erlzmq:send(Socket, list_to_binary(Line)),
    {reply, ok, Socket};

%% handle complete messages (username, message, timestamp) sent to the socket
%% from the GUI
handle_call({msg, {Name, Msg, Time}}, _From, Socket) ->
    StrMsg = string:join(
        [
            string:join([Time, Name], " "),
            Msg
        ],
        ": "
    ),
    erlzmq:send(Socket, list_to_binary(StrMsg)),
    {reply, ok, Socket}.

%% unused function; included for consistency with gen server model
handle_cast(_A, _B) ->
    ok.

%% close socket
terminate(_A, Socket) ->
    erlzmq:close(Socket),
    ok.

%% stop client
stop() ->
    gen_server:stop({local, ?SERVER}).

%% unused gen_server functions included to prevent warnings
start_link() -> ok.
handle_info(_,_) -> ok.
code_change(_,_,_) -> ok.
