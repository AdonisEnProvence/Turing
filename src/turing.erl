-module(turing).
-include("machine.hrl").

-export([main/1]).

print_usage() ->
    io:format(
        "usage: ft_turing COMMAND\n"
        "\n"
        "commands:\n"
        "  serve          starts the turing machine http REST API\n"
        "  run            runs cli turing machine command\n"
        "\n"
        "Run 'ft_turing COMMAND --help' for more information on a command.~n"
    ).

main([SubCommand | SubCommandArgs]) ->
    try
        case SubCommand of
            "run" ->
                cli:run_cli_command(SubCommandArgs);
            "serve" ->
                {ok, _} = application:ensure_all_started(cowboy),

                PoolMasterWorkerPid = spawn(pool_worker_master, init_pool_worker_master, []),
                Dispatch = cowboy_router:compile([
                    {'_', [
                        {<<"/execute-machine">>, execute_machine_handler, [PoolMasterWorkerPid]}
                    ]}
                ]),

                {ok, _} = cowboy:start_clear(
                    ping_listener,
                    [{port, 8080}],
                    #{
                        % cowboy_router and cowboy_handler are the default
                        % middlewares for a cowboy application.
                        % We need to set them when overriding middlewares list.
                        middlewares => [cors_middleware, cowboy_router, cowboy_handler],
                        env => #{dispatch => Dispatch}
                    }
                ),
                io:format("Server starting~n"),
                receive
                    quit ->
                        ok = cowboy:stop_listener(http)
                end;
            _ ->
                print_usage()
        end
    of
        _ -> erlang:halt(0)
    catch
        Throw ->
            io:format("Caught unexpected error:~n~p~n", [Throw]),
            erlang:halt(0)
    end;
main(_) ->
    print_usage(),
    erlang:halt(0).
