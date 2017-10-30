% Quinn Collins

-module(chat).
-export([init/1, handle_cast/2, handle_call/3, terminate/2]).
-export([list/1, send/3, join/3, start_link/1, stop/1]).

% -----------------------------------------------------------------------------
%                       Server Functions
% -----------------------------------------------------------------------------

% initialize server state as an empty map
init(_Arguments) ->
    {ok, maps:new()}.

% list users
handle_call(list, _Pid, State) ->
    {reply, maps:values(State), State}.

% tell server how to subscribe user to chat server
handle_cast({subscribe, {Pid, Username}}, State) ->
    {noreply, maps:put(Pid, Username, State)};

% tell server how to unsubscribe user from chat server
handle_cast({unsubscribe, ReceiverPid}, State) ->
    {noreply, maps:remove(ReceiverPid, State)};

% send message to all users in the chat
handle_cast({send, Sender, Message}, State) ->
    maps:map(fun(Pid, _Username) ->
        Pid ! {send, Sender, Message}
    end, State),
    {noreply, State}.

% tell server how to stop
terminate(Reason, _State) ->
    exit(self(), Reason).

% -----------------------------------------------------------------------------
%                       Client Functions
% -----------------------------------------------------------------------------

% start server running locally on the chat module, with no arguments or options
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

% client fxn to subscribe user
subscribe(ServerRef, Name) ->
    gen_server:cast(ServerRef, {subscribe, Name}).

% client fxn to unsubscribe user
unsubscribe(ServerRef, ReceiverPid) ->
    gen_server:cast(ServerRef, {unsubscribe, ReceiverPid}).

% client fxn to send message to other users
send(ServerRef, Sender, Message) ->
    gen_server:cast(ServerRef, {send, Sender, Message}).

% client fxn to list 
list(ServerRef) ->
    io:format("~p~n", [gen_server:call(ServerRef, list)]).

% client fxn to join chat: specify username, node the chat is running on, and
% name of the chat server
join(Username, ServerNode, ServerName) ->
    Receiver = spawn(fun() -> read_loop() end),
    subscribe({ServerName, ServerNode}, {Receiver, Username}),
    write_loop({ServerName, ServerNode}, Username, Receiver).

% client fxn to receive messages from the chat server
read_loop() ->
    receive
        {send, Username, Message} -> io:format("~p: ~p~n", [Username, Message])
    end,
    read_loop().

% client fxn to write messages to the chat server
write_loop(ServerRef, Username, Receiver) ->
    Message = io:get_line("WRITE: "),
    case(Message) of
        "--quit\n" -> unsubscribe(ServerRef, Receiver), ok; 
        "--list\n" -> list(ServerRef),
                      write_loop(ServerRef, Username, Receiver);   
        _          -> send(ServerRef, Username, Message), 
                      write_loop(ServerRef, Username, Receiver)
    end.

% client fxn to stop the server
stop(ServerRef) ->
    gen_server:stop(ServerRef).

