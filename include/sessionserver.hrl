%%%-------------------------------------------------------------------
%%% @author ssobko
%%% @copyright (C) 2014, The Profitware Group
%%% @doc
%%% Common definitions for sessionserver application
%%% @end
%%% Created : 18.10.2014 18:33
%%%-------------------------------------------------------------------
-author("ssobko").

-define(CRLF, [10]).
-define(SOCKETSUPERVISOR, sessionserver_socket_sup).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% Default values for supervisors
-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).