%%%-------------------------------------------------------------------
%%% @author ssobko
%%% @copyright (C) 2014, The Profitware Group
%%% @doc
%%%
%%% @end
%%% Created : 15.10.2014 15:53
%%%-------------------------------------------------------------------

-module(sessionserver_socket_bridge).
-author("ssobko").

-behaviour(supervisor_bridge).

-export([start_link/1]).

% supervisor_bridge export
-export([init/1, terminate/2]).

% internal proc_lib:start_link
-export([accept_loop/1]).

-export([connection_handler/1]).

%% Definitions
-define(SERVER, ?MODULE).
-define(CRLF, [10]).

connection_handler(ClientSocket) ->
    case gen_tcp:recv(ClientSocket, 0) of
        {ok, Packet} ->
            Message = sessionserver:dispatch(Packet) ++ ?CRLF,
            gen_tcp:send(ClientSocket, Message);
        {error, Reason} ->
            error(Reason)
    end,
    connection_handler(ClientSocket).

start_link(ListenSocket) ->
    io:format("Socket bridge ~p ~w~n", [self(), ListenSocket]),
    supervisor_bridge:start_link({local, ?SERVER}, ?SERVER, [ListenSocket]).

init([ListenSocket]) ->
    io:format("Acceptor ~p ~w~n", [self(), ListenSocket]),
    proc_lib:start_link(?SERVER, accept_loop, [ListenSocket], infinity).

terminate(_Reason, ListenSocket) ->
    gen_tcp:close(ListenSocket).

accept_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, ClientSocket} ->
            proc_lib:start_link(sessionserver_socket_sup, create_acceptor, [self(), ListenSocket]),
            io:format("Client socket ~p ~w~n", [self(), ClientSocket]),
            Pid = proc_lib:start_link(?SERVER, connection_handler, [ClientSocket]),
            gen_tcp:controlling_process(ClientSocket, Pid),
            io:format("Hello~n"),
            ok;
        {error, closed} ->
            error({shutdown, tcp_closed});
        {error, Reason} ->
            error(Reason)
    end.