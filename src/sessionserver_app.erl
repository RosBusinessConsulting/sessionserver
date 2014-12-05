%%%-------------------------------------------------------------------
%%% @author Sergey Sobko <ssobko@rbc.ru>
%%% @copyright (C) 2014, RosBusinessConsulting
%%% @doc
%%% Sessionserver application
%%% @end
%%% Created : 15.10.2014 15:53
%%%-------------------------------------------------------------------

-module(sessionserver_app).
-author("ssobko").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    sessionserver_sup:start_link().

stop(_State) ->
    ok.
