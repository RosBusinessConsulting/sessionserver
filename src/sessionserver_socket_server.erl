%%%-------------------------------------------------------------------
%%% @author ssobko
%%% @copyright (C) 2014, The Profitware Group
%%% @doc
%%%
%%% @end
%%% Created : 15.10.2014 15:53
%%%-------------------------------------------------------------------

-module(sessionserver_socket_server).
-author("ssobko").

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([accept_socket/2, handle_socket/2]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% Definitions
-define(SERVER, ?MODULE).
-define(CRLF, [10]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(ListenSocket) ->
    gen_server:start_link(?SERVER, [ListenSocket], []).

accept_socket(Pid, ListenSocket) ->
    gen_server:cast(Pid, {accept, Pid, ListenSocket}).

handle_socket(Pid, ClientSocket) ->
    gen_server:cast(Pid, {handler, Pid, ClientSocket}).

%% ===================================================================
%% Server callbacks
%% ===================================================================

init([ListenSocket]) ->
    accept_socket(self(), ListenSocket),
    {ok, []}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({handler, Pid, ClientSocket}, State) ->
    gen_tcp:controlling_process(ClientSocket, Pid),
    case gen_tcp:recv(ClientSocket, 0) of
        {ok, Packet} ->
            Message = sessionserver:dispatch(Packet) ++ ?CRLF,
            gen_tcp:send(ClientSocket, Message);
        {error, Reason} ->
            sessionserver_socket_sup:terminate_acceptor(Pid),
            error(Reason)
    end,
    gen_server:cast(Pid, {handler, Pid, ClientSocket}),
    {noreply, State};

handle_cast({accept, Pid, ListenSocket}, State) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, ClientSocket} ->
            proc_lib:start_link(sessionserver_socket_sup, create_acceptor, [Pid, ListenSocket]),
            handle_socket(Pid, ClientSocket),
            ok;
        {error, closed} ->
            error({shutdown, tcp_closed});
        {error, Reason} ->
            error(Reason)
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.