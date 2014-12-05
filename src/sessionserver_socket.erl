%%%-------------------------------------------------------------------
%%% @author Sergey Sobko <ssobko@rbc.ru>
%%% @copyright (C) 2014, RosBusinessConsulting
%%% @doc
%%% Server TCP connection initialization
%%% @end
%%% Created : 15.10.2014 15:53
%%%-------------------------------------------------------------------

-module(sessionserver_socket).
-author("ssobko").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% Definitions
-include_lib("sessionserver/include/sessionserver.hrl").
-define(SERVER, ?MODULE).

-record(state, {acceptors, options}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

%% ===================================================================
%% Server callbacks
%% ===================================================================

init([]) ->
    init_ranch(),
    start_socket(),
    {ok, init_state()}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(socket, State) ->
    Acceptors = get_state_acceptors(State),
    Options = get_state_options(State),
    {ok, _} = ranch:start_listener(sessionserver, Acceptors, ranch_tcp, Options, sessionserver_protocol, []),
    {noreply, State};

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

init_ranch() ->
    application:start(ranch).

-spec start_socket() -> ok.
start_socket() ->
    gen_server:cast(?SERVER, socket).

-spec init_state() -> #state{}.
init_state() ->
    #state{
        acceptors = sessionserver_config:get(tcp, acceptors),
        options = sessionserver_config:get(tcp, options)
    }.

-spec get_state_options(#state{}) -> [{atom(), any()} | {raw, any(), any(), any()}].
get_state_options(State) ->
    State#state.options.

-spec get_state_acceptors(#state{}) -> integer().
get_state_acceptors(State) ->
    State#state.acceptors.
