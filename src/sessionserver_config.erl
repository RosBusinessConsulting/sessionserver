%%%-------------------------------------------------------------------
%%% @author ssobko
%%% @copyright (C) 2014, The Profitware Group
%%% @doc
%%% Configuration manager
%%% @end
%%% Created : 19.10.2014 17:22
%%%-------------------------------------------------------------------
-module(sessionserver_config).
-author("ssobko").

-behaviour (gen_server).

%% API
-export([start_link/0]).
-export([get/2]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% Definitions
-include_lib("sessionserver/include/sessionserver.hrl").
-define (SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get(Group :: atom(), Key :: atom()) -> any().
get(Group, Key) ->
    gen_server:call(?SERVER, {get, Group, Key}).

%% ===================================================================
%% Server callbacks
%% ===================================================================

init(_Args) ->
    {ok, state}.

handle_call({get, Group, Key}, _From, State) ->
    Reply = case application:get_env(sessionserver, Group) of
        undefined ->
            {error, nogroup, Group}; % Don't delete groups from configuration!
        {ok, Found} ->
            proplists:get_value(Key, Found, default(Group, Key))
    end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec default(Group :: atom(), Key :: atom()) -> any().
default(tcp, port) -> 8080;
default(tcp, options) ->
    [
        list,
        {packet, line},
        {active, false},
        {reuseaddr, true}
    ];
default(tcp, timeout) -> 30000;
default(_, _) -> [].