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

-export([start_link/0, start_socket/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% Definitions
-define(SERVER, ?MODULE).
-define(CRLF, [10]).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

init([]) ->
    {ok, []}.

%% ===================================================================
%% API functions
%% ===================================================================

start_socket() ->
    gen_server:cast(?SERVER, socket).

%% ===================================================================
%% Server callbacks
%% ===================================================================

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(socket, State) ->
    Port = 5678,
    Options = [list, {packet, line}, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Options) of
        {ok, ListenSocket} ->
            io:format("Created ListenSocket ~p ~w~n", [self(), ListenSocket]),
            supervisor:start_child(sessionserver_socket_sup, [ListenSocket]);
        OtherResult -> OtherResult
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.