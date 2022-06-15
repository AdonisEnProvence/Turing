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
            ping_server_app:start(undefined, undefined);
        _ ->
            print_usage()
    end,
    erlang:halt(0);
main(_) ->
    print_usage(),
    erlang:halt(0).
