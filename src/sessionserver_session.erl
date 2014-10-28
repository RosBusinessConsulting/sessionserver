%%%-------------------------------------------------------------------
%%% @author ssobko
%%% @copyright (C) 2014, The Profitware Group
%%% @doc
%%% Session utilities
%%% @end
%%% Created : 15.10.2014 15:53
%%%-------------------------------------------------------------------

-module(sessionserver_session).
-author("ssobko").

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get_new_session/0]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% Definitions
-include_lib("sessionserver/include/sessionserver.hrl").
-define(SERVER, ?MODULE).

-record(state, {letters, length}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

get_new_session() ->
    gen_server:call(?SERVER, get_new_session).

%% ===================================================================
%% Server callbacks
%% ===================================================================

init([]) ->
    {ok, init_state()}.

handle_call(get_new_session, _From, State) ->
    SessionString = get_random_string(get_state_length(State), get_state_letters(State)),
    Reply = case catch list_to_existing_atom(SessionString) of
        {'EXIT', {badarg, _Info}} ->
            list_to_atom(SessionString);
        Others ->
            Others
    end,
    {reply, Reply, State};

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

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec init_state() -> #state{}.
init_state() ->
    Letters = lists:flatten([lists:seq($A, $Z), lists:seq($a, $z), lists:seq($0, $9)]),
    Length = sessionserver_config:get(session, length),
    #state{
        letters = Letters,
        length = Length
    }.

-spec get_random_string(Length :: integer(), AllowedChars :: string()) -> string().
get_random_string(Length, AllowedChars) ->
    random:seed(now()),
    Size = length(AllowedChars),
    [lists:nth(random:uniform(Size), AllowedChars) || _ <- lists:seq(1, Length)].

-spec get_state_letters(#state{}) -> string().
get_state_letters(State) ->
    State#state.letters.

-spec get_state_length(#state{}) -> integer().
get_state_length(State) ->
    State#state.length.