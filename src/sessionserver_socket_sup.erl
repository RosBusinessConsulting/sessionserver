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
-export([start_link/0]).
-export([create_acceptor/2, terminate_acceptor/1, terminate_acceptors/0, terminate_all_by_childspec/1]).

%% Supervisor callbacks
-export([init/1]).

%% Definitions
-include_lib("sessionserver/include/sessionserver.hrl").
-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?SERVER, []).

create_acceptor(ParentPid, ListenSocket) ->
    proc_lib:init_ack(ParentPid, {ok, self()}),
    supervisor:start_child(?SERVER, [ListenSocket]).

terminate_acceptor(ParentPid) ->
    proc_lib:init_ack(ParentPid, {ok, self()}),
    supervisor:terminate_child(?SERVER, ParentPid).

terminate_all_by_childspec([{_Id, Pid, _Type, _Modules} | Tail]) ->
    proc_lib:start_link(?SERVER, terminate_acceptor, [Pid]),
    terminate_all_by_childspec(Tail);

terminate_all_by_childspec([]) ->
    ok.

terminate_acceptors() ->
    terminate_all_by_childspec(supervisor:which_children(?SERVER)).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Flags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
    Spec = [
        ?CHILD(sessionserver_socket_server, worker, transient)
    ],
    {ok, {Flags, Spec}}.
