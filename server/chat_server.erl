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

%%--------------------------------------------------------------------
%% @doc
%% Starts/stops the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Room) ->
    gen_server:start_link({global, Room}, chat_server, [], []).

stop(Room) ->
    gen_server:stop({global, Room}).

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
    {ok, {[], [], []}}.

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
handle_call({subscribers}, _From, {CurrSubs, Pids, Transcript}) ->
    {reply, CurrSubs, {CurrSubs, Pids, Transcript}};

handle_call({subscribe, Username, Pid}, _From, {CurrSubs, Pids, Transcript}) ->
    {reply, Transcript, {[Username | CurrSubs], [Pid | Pids], Transcript}};

handle_call({unsubscribe, Username, Pid}, _From, 
            {CurrentSubs, Pids, Transcript}) ->
    {RestUsers, RestPids} = unsubscribe(Username, Pid, CurrentSubs, Pids),
    {reply, Transcript, {RestUsers, RestPids, Transcript}}.

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
handle_cast({send, Username, Msg, Time}, {CurrentSubs, Pids, Transcript}) ->
    send_msg_to_all(Username, Msg, Time, Pids),
    {noreply, {CurrentSubs, Pids, [{Username, Msg, Time} | Transcript]}}.

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
send_msg_to_all(_Username, _Msg, _Time, []) ->
    ok;
send_msg_to_all(Username, Msg, Time, [Pid | Tail]) ->
    Pid ! {message, Username, Msg, Time},
    send_msg_to_all(Username, Msg, Time, Tail).
