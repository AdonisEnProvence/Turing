%%%-------------------------------------------------------------------
%% @doc ping_server public API
%% @end
%%%-------------------------------------------------------------------

-module(ping_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _} = application:ensure_all_started(cowboy),

    Dispatch = cowboy_router:compile([
        {<<"localhost">>, [{<<"/ping">>, ping_handler, []}]}
    ]),

    {ok, _} = cowboy:start_clear(
        ping_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    {ok, Pid} = ping_server_sup:start_link(),
    io:format("Server starting pid = ~p ~n", [Pid]),
    timer:sleep(100000).

stop(_State) ->
    ok = cowboy:stop_listener(http).

%% internal functions
