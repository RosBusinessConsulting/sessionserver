%%%-------------------------------------------------------------------
%%% @author ssobko
%%% @copyright (C) 2014, The Profitware Group
%%% @doc
%%%
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

-record(state, {socket}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

%% ===================================================================
%% Server callbacks
%% ===================================================================

init([]) ->
    start_socket(),
    {ok, #state{socket=null}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(socket, State) ->
    Port = sessionserver_config:get(tcp, port),
    Options = sessionserver_config:get(tcp, options),
    NewState = case gen_tcp:listen(Port, Options) of
        {ok, ListenSocket} ->
            proc_lib:start_link(?SOCKETSUPERVISOR, create_acceptor, [self(), ListenSocket]),
            State#state{socket=ListenSocket};
        OtherResult ->
            error(OtherResult)
    end,
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    Socket = State#state.socket,
    case Socket of
        null ->
            ok;
        ListenSocket ->
            gen_tcp:close(ListenSocket)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec start_socket() -> ok.
start_socket() ->
    gen_server:cast(?SERVER, socket).