%%%-------------------------------------------------------------------
%%% @author ssobko
%%% @copyright (C) 2014, The Profitware Group
%%% @doc
%%%
%%% @end
%%% Created : 15.10.2014 15:53
%%%-------------------------------------------------------------------

-module(sessionserver_socket_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_socket/0, create_acceptor/1]).

%% Supervisor callbacks
-export([init/1]).

%% Definitions
-define(SERVER, ?MODULE).
-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Params), {I, {I, start_link, Params}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?SERVER, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Flags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
    Spec = [
        ?CHILD(sessionserver_socket_bridge, worker, [])
    ],
    {ok, {Flags, Spec}}.

create_acceptor(ListenSocket) ->
    io:format("Create acceptor ~p ~w~n", [self(), ListenSocket]),
    supervisor:start_child(?SERVER, [ListenSocket]).

start_socket() ->
    Port = 5678,
    Options = [list, {packet, line}, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Options) of
        {ok, ListenSocket} ->
            io:format("Created ListenSocket ~p ~w~n", [self(), ListenSocket]),
            create_acceptor(ListenSocket);
        OtherResult -> OtherResult
    end.
