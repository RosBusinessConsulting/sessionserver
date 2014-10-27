%%%-------------------------------------------------------------------
%%% @author ssobko
%%% @copyright (C) 2014, The Profitware Group
%%% @doc
%%%
%%% @end
%%% Created : 15.10.2014 15:53
%%%-------------------------------------------------------------------

-module(sessionserver_protocol).
-author("ssobko").

-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API
-export([start_link/4]).
-export([handle_socket/2]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-export([init/4]).

%% Definitions
-include_lib("sessionserver/include/sessionserver.hrl").
-define(SERVER, ?MODULE).

-record(state, {socket, transport, timeout}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

handle_socket(Pid, Data) ->
    gen_server:cast(Pid, {handler, Data}).

%% ===================================================================
%% Server callbacks
%% ===================================================================

init([]) ->
    {ok, undefined}.

init(Ref, Socket, Transport, _Opts = []) ->
	ok = proc_lib:init_ack({ok, self()}),
	ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    Timeout = sessionserver_config:get(tcp, timeout),
	gen_server:enter_loop(?MODULE, [], #state{socket=Socket, transport=Transport, timeout=Timeout}, Timeout).

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({handler, Data}, State = #state{socket=Socket, transport=Transport, timeout=_Timeout}) ->
    Transport:setopts(Socket, [{active, once}]),
    {Type, Message} = sessionserver:dispatch(binary_to_list(Data)),
    case Type of
        message ->
            Transport:send(Socket, Message ++ ?CRLF),
            ok;
        close ->
            Transport:send(Socket, Message ++ ?CRLF),
            error(Message);
        skip ->
            ok
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, State = #state{socket=Socket, transport=_Transport, timeout=Timeout}) ->
    handle_socket(self(), Data),
	{noreply, State, Timeout};

handle_info({tcp_closed, _Socket}, State) ->
	{stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
	{stop, Reason, State};

handle_info(timeout, State) ->
	{stop, normal, State};

handle_info(_Info, State) ->
	{stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
