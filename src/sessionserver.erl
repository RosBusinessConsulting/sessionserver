%%%-------------------------------------------------------------------
%%% @author ssobko
%%% @copyright (C) 2014, The Profitware Group
%%% @doc
%%%
%%% @end
%%% Created : 15.10.2014 15:53
%%%-------------------------------------------------------------------

-module(sessionserver).
-author("ssobko").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([test/0, dispatch/1]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% Definitions
-define(SERVER, ?MODULE).
-define(CRLF, [10]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

test() ->
    gen_server:call(?SERVER, test).

dispatch(Packet) ->
    gen_server:call(?SERVER, {packet, Packet}).

%% ===================================================================
%% Server callbacks
%% ===================================================================

init([]) ->
    {ok, []}.

handle_call(test, _From, State) ->
    io:format("Got test message!~n", []),
    {reply, ok, State};

handle_call({packet, "PING" ++ ?CRLF}, _From, State) ->
    Message = "PONG",
    {reply, Message, State};

handle_call({packet, _Packet}, _From, State) ->
    Message = "UNRECOGNIZED COMMAND",
    {reply, Message, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.