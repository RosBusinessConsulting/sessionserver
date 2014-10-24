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
    {ok, init_state()}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(socket, State) ->
    Port = sessionserver_config:get(tcp, port),
    Options = sessionserver_config:get(tcp, options),
    NewState = case gen_tcp:listen(Port, Options) of
        {ok, ListenSocket} ->
            proc_lib:start_link(?SOCKETSUPERVISOR, create_acceptor, [self(), ListenSocket]),
            set_state(State, ListenSocket);
        OtherResult ->
            error(OtherResult)
    end,
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    Socket = get_state_socket(State),
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

-spec init_state() -> #state{}.
init_state() ->
    #state{socket = null}.

-spec get_state_socket(#state{}) -> gen_tcp:socket().
get_state_socket(State) ->
    State#state.socket.

-spec set_state(#state{}, gen_tcp:socket()) -> #state{}.
set_state(State, Socket) ->
    State#state{socket = Socket}.