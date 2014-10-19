%%%-------------------------------------------------------------------
%%% @author ssobko
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. окт 2014 17:22
%%%-------------------------------------------------------------------
-module(sessionserver_config).
-author("ssobko").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get/2]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% Definitions
-include_lib("sessionserver/include/sessionserver.hrl").
-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

get(Group, Key) ->
    gen_server:call(?SERVER, {get, Group, Key}).

init([]) ->
    {ok, []}.

handle_cast(_Params, State) ->
    {noreply, State}.

handle_call({get, Group, Key}, _From, State) ->
    proplists:get_value({Group, Key}, [], []);

handle_call(_Params, _From, State) ->
    {reply, ok, State}.

handle_info(_Params, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
