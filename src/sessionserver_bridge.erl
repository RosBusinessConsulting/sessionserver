%%%-------------------------------------------------------------------
%%% @author ssobko
%%% @copyright (C) 2014, The Profitware Group
%%% @doc
%%%
%%% @end
%%% Created : 15.10.2014 15:53
%%%-------------------------------------------------------------------

-module(sessionserver_bridge).
-author("ssobko").

-behaviour(supervisor_bridge).

-export([start_link/0]).

% supervisor_bridge export
-export([init/1, terminate/2]).

% internal proc_lib:start_link
-export([accept_init/2]).

-export([connection_handler/1]).

%% Definitions
-define(SERVER, ?MODULE).

connection_handler(ClientSocket) ->
    case gen_tcp:recv(ClientSocket, 0) of
        {ok, Packet} ->
            io:format("Packet ~w~n", [Packet]),
            gen_tcp:send(ClientSocket, Packet);
        {error, Reason} ->
            error(Reason)
    end,
    connection_handler(ClientSocket).

start_link() ->
    Port = 5678,
    Options = [binary, {packet, 0}, {active, false}],
    SocketOptions = {Port, Options},
    supervisor_bridge:start_link({local, ?SERVER}, ?SERVER, SocketOptions).

init({Port, Options}) ->
    case gen_tcp:listen(Port, Options) of
        {ok, ListenSocket} ->
            {ok, ServerPid} = proc_lib:start_link(?SERVER, accept_init,
                [self(), ListenSocket], 1000),
            {ok, ServerPid, ListenSocket};
        OtherResult -> OtherResult
    end.

terminate(_Reason, ListenSocket) ->
    gen_tcp:close(ListenSocket).

accept_init(ParentPid, ListenSocket) ->
    proc_lib:init_ack(ParentPid, {ok, self()}),
    accept_loop(ListenSocket).

accept_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, ClientSocket} ->
            Pid = proc_lib:spawn_link(?SERVER, connection_handler, [ClientSocket]),
            ok = gen_tcp:controlling_process(ClientSocket, Pid),
            accept_loop(ListenSocket);
        {error, closed} ->
            error({shutdown, tcp_closed});
        {error, Reason} ->
            error(Reason)
    end.