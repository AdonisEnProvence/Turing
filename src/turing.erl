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
    case SubCommand of
        "run" ->
            cli:run_cli_command(SubCommandArgs);
        "serve" ->
            {ok, _} = application:ensure_all_started(cowboy),

            Dispatch = cowboy_router:compile([
                {<<"localhost">>, [
                    {<<"/execute-machine">>, execute_machine_handler, []}
                ]}
            ]),

            {ok, _} = cowboy:start_clear(
                ping_listener,
                [{port, 8080}],
                #{env => #{dispatch => Dispatch}}
            ),
            io:format("Server starting~n"),
            receive
                quit ->
                    ok = cowboy:stop_listener(http)
            end;
        _ ->
            print_usage()
    end,
    erlang:halt(0);
main(_) ->
    print_usage(),
    erlang:halt(0).
