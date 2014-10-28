%%%-------------------------------------------------------------------
%%% @author ssobko
%%% @copyright (C) 2014, The Profitware Group
%%% @doc
%%% Main application supervisor.
%%% @end
%%% Created : 15.10.2014 15:53
%%%-------------------------------------------------------------------

-module(sessionserver_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Flags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
    Spec = [
        ?CHILD(sessionserver_config, worker, permanent),
        ?CHILD(sessionserver_session, worker, permanent),
        ?CHILD(sessionserver_db, worker, permanent),
        ?CHILD(sessionserver, worker, permanent),
        ?CHILD(sessionserver_socket, worker, permanent)
    ],
    {ok, {Flags, Spec}}.
