%%%-------------------------------------------------------------------
%%% @author Minh D. Nguyen <mnguye17@vm-hw00.eecs.tufts.edu>
%%% @copyright (C) 2017, Minh D. Nguyen
%%% @doc
%%%
%%% @end
%%% Created : 27 Sep 2017 by Minh D. Nguyen <mnguye17@vm-hw00.eecs.tufts.edu>
%%%-------------------------------------------------------------------
-module(chat_server).

-behavior(gen_server).

%% API
-export([start_link/1, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         terminate/2]).

%%%===================================================================
%%% API
%%%===================================================================

%% Starts the server
start_link(Room) ->
    gen_server:start_link({global, Room}, chat_server, [], []).

%% Stop the server
stop(Room) ->
    gen_server:stop({global, Room}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% Initialize the server. State is {[usernames], [PIDs], [Transcript]}
init([]) ->
    {ok, {[], [], []}}.

%% Handle calls to add and remove subscribers, return list of subscribers
handle_call({subscribers}, _From, {CurrSubs, Pids, Transcript}) ->
    {reply, CurrSubs, {CurrSubs, Pids, Transcript}};

handle_call({subscribe, Username, Pid}, _From, {CurrSubs, Pids, Transcript}) ->
    {reply, Transcript, {[Username | CurrSubs], [Pid | Pids], Transcript}};

handle_call({unsubscribe, Username, Pid}, _From, 
            {CurrentSubs, Pids, Transcript}) ->
    {RestUsers, RestPids} = unsubscribe(Username, Pid, CurrentSubs, Pids),
    {reply, Transcript, {RestUsers, RestPids, Transcript}}.

%% Handling cast to send a new message to all subscribers
handle_cast({send, Username, Msg, Time}, {CurrentSubs, Pids, Transcript}) ->
    send_msg_to_all(Username, Msg, Time, Pids),
    {noreply, {CurrentSubs, Pids, [{Username, Msg, Time} | Transcript]}}.

%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
terminate(_Reason, _CurrState) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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

%% Given a message and its sender along with a list of chat room subscribers,
%% send a line of the format "sender: message" to all subscribers
send_msg_to_all(_Username, _Msg, _Time, []) ->
    ok;
send_msg_to_all(Username, Msg, Time, [Pid | Tail]) ->
    Pid ! {message, Username, Msg, Time},
    send_msg_to_all(Username, Msg, Time, Tail).
