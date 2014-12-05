%%%-------------------------------------------------------------------
%%% @author Sergey Sobko <ssobko@rbc.ru>
%%% @copyright (C) 2014, RosBusinessConsulting
%%% @doc
%%% Database backend
%%% @end
%%% Created : 25.10.2014 01:45
%%%-------------------------------------------------------------------
-module(sessionserver_db).
-author("ssobko").

-behaviour (gen_server).

%% API
-export([start_link/0]).
-export([check_user/2, update_user/4, check_session/1]).

%% Server callbacks
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% Definitions
-include_lib("sessionserver/include/sessionserver.hrl").
-define(SERVER, ?MODULE).
-define(TABLE, sessionserver_user).

-record(?TABLE, {login, password, groups = [], session = null}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec check_user(Login :: atom(), Password :: atom()) -> {ok, term()} | {error, term()}.
check_user(Login, Password) ->
    gen_server:call(?SERVER, {check_user, Login, Password}).

-spec check_session(Session :: atom()) -> {ok, term()} | {error, term()}.
check_session(Session) ->
    gen_server:call(?SERVER, {check_session, Session}).

-spec update_user(Login :: atom(), Password :: atom(), Groups :: list(), Session :: atom()) ->
    {ok, term()} | {error, term()}.
update_user(Login, Password, Groups, Session) ->
    gen_server:call(?SERVER, {update_user, Login, Password, Groups, Session}).

%% ===================================================================
%% Server callbacks
%% ===================================================================

init(_Args) ->
    init_mnesia(),
    {ok, state}.

handle_call({check_user, Login, Password}, _From, State) ->
    Fun = fun() ->
        mnesia:read(?TABLE, Login)
    end,
    Reply = case mnesia:transaction(Fun) of
        {atomic, []} ->
            {error, invalid_login};
        {atomic, [{?TABLE, Login, Password, Groups, Session}]} ->
            {ok, {Login, Password, Groups, Session}};
        {atomic, [{?TABLE, Login, _WrongPassword, _Groups, _Session}]} ->
            {error, invalid_password};
        Others ->
            {error, {something_wrong, Others}}
    end,
    {reply, Reply, State};

handle_call({check_session, Session}, _From, State) ->
    Fun = fun() ->
        mnesia:match_object(set_record('_', '_', '_', Session))
    end,
    Reply = case mnesia:transaction(Fun) of
        {atomic, [{?TABLE, Login, Password, Groups, Session}]} when Session =/= null ->
            {ok, {Login, Password, Groups, Session}};
        {atomic, _Others} ->
            {error, invalid_session};
        Others ->
            {error, {something_wrong, Others}}
    end,
    {reply, Reply, State};

handle_call({update_user, Login, Password, Groups, Session}, _From, State) ->
    Fun = fun() ->
        mnesia:write(set_record(Login, Password, Groups, Session))
    end,
    Reply = mnesia:transaction(Fun),
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

-spec init_mnesia() -> ok.
init_mnesia() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(?TABLE, [
        {disc_copies, [node()] },
        {attributes, record_info(fields, ?TABLE)}
    ]),
    ok.

-spec set_record(Login :: atom(), Password :: atom(), Groups :: list(), Session :: atom()) -> #?TABLE{}.
set_record(Login, Password, Groups, Session) ->
    #?TABLE{
        login = Login,
        password = Password,
        groups = Groups,
        session = Session
    }.