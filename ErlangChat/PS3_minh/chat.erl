%%%-------------------------------------------------------------------
%%% @author Minh D. Nguyen <mnguye17@vm-hw00.eecs.tufts.edu>
%%% @copyright (C) 2017, Minh D. Nguyen
%%% @doc
%%%
%%% @end
%%% Created : 27 Sep 2017 by Minh D. Nguyen <mnguye17@vm-hw00.eecs.tufts.edu>
%%%-------------------------------------------------------------------
-module(chat).

-behavior(gen_server).

%% API
-export([start_link/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         terminate/2]).

%% Client functions
-export([join_room/3, get_message/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts/stops the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Room) ->
    gen_server:start_link({local, Room}, chat, [], []).

stop(Room) ->
    gen_server:stop(Room).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    % io:format('~p~n', [Room]),
    {ok, {[], []}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({subscribe, Pid, Username}, _From, {CurrSubs, Pids}) ->
    {reply, ok, {[Username | CurrSubs], [Pid | Pids]}};

handle_call({subscribers}, _From, {CurrSubs, Pids}) ->
    {reply, CurrSubs, {CurrSubs, Pids}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({send, Username, Msg}, {CurrentSubs, Pids}) ->
    send_msg_to_all(Username, Msg, Pids),
    {noreply, {CurrentSubs, Pids}};

handle_cast({unsubscribe, Username, Pid}, {CurrentSubs, Pids}) ->
    NewState = unsubscribe(Username, Pid, CurrentSubs, Pids),
    {noreply, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _CurrState) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% unsubscribe
%% Given the username and the pid, remove the matching username and pid
%% from the lists
unsubscribe(_User, _Pid, [], []) ->
    {[], []};
%% Both the User and the Pid has to match
unsubscribe(User, Pid, [User | RestUsers], [Pid | RestPids]) ->
    {RestUsers, RestPids};
unsubscribe(User, Pid, [FirstUser | RestUsers], [FirstPid | RestPids]) ->
    {Users, Pids} = unsubscribe(User, Pid, RestUsers, RestPids),
    {[FirstUser | Users], [FirstPid | Pids]}.

%% send_msg_to_all
%% Given a message and its sender along with a list of chat room subscribers,
%% send a line of the format "sender: message" to all subscribers
send_msg_to_all(_Username, _Msg, []) ->
    ok;
send_msg_to_all(Username, Msg, [Pid | Tail]) ->
    Pid ! {message, Username, Msg},
    send_msg_to_all(Username, Msg, Tail).

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
    Line = io:get_line("Enter a message: "),
    if
        Line == "--quit\n" ->
            gen_server:cast({Room, ServNode}, {unsubscribe, Username, RecPid}),
            ok;
        Line == "--list\n" ->
            AllSubs = gen_server:call({Room, ServNode}, {subscribers}),
            io:format("Subscribers: ~p~n", [AllSubs]),
            send_message(Username, RecPid, ServNode, Room);
        true ->
            gen_server:cast({Room, ServNode}, {send, Username, Line}),
            send_message(Username, RecPid, ServNode, Room)
    end.

%% Exported Client Functions
join_room(ServerNode, Room, Username) ->
    Pid = spawn(chat, get_message, []),
    gen_server:call({Room, ServerNode}, {subscribe, Pid, Username}),
    send_message(Username, Pid, ServerNode, Room).
