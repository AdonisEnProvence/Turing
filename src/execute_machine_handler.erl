-module(execute_machine_handler).
-behaviour(cowboy_handler).
-export([init/2]).

pretty_value(List) -> lists:flatten(io_lib:format("~p", [List])).

get_decoded_body(Req0) ->
    ReadBodyResult = cowboy_req:read_body(Req0),
    case ReadBodyResult of
        {ok, Data, _Req} ->
            decode_body(Data);
        {error, _} ->
            {error, invalid_body}
    end.

decode_body(RawBody) ->
    TryDecodedReq = jsone:try_decode(RawBody),
    case TryDecodedReq of
        {ok, DecodedReqBody, _} ->
            io:format("decoded successfully the body~n"),
            io:format("~p~n", [pretty_value(DecodedReqBody)]),
            {ok, DecodedReqBody};
        {error, Error} ->
            io:format("failure decoding the body~n"),
            io:format("~p~n", [Error]),
            {error, invalid_body}
    end.

parse_validate_and_execute(RawMachineConfig, RawInput) ->
    StringInput = binary_to_list(RawInput),
    ParserValidatorResult = parse_and_validate_machine_config:parse_and_validate_decoded_machine_and_input(
        RawMachineConfig, StringInput
    ),

    case ParserValidatorResult of
        {ok, ParsedMachineConfig, ParsedInput} ->
            io:format("Interpreter starting...~n"),
            interpreter:start(ParsedMachineConfig, ParsedInput, fun(
                {Tape, _CurrentState, _IndexOnTape, _Status, _Transition}
            ) ->
                io:format("~p~n", [Tape])
            end),
            io:format("Interpreter closing...~n"),
            {ok, "cocorico"};
        {error, FormattedError} ->
            {error, FormattedError}
    end.

reply_error(Req0, Message) ->
    Req = cowboy_req:reply(
        400,
        #{<<"content-type">> => <<"text/plain">>},
        list_to_binary(Message),
        Req0
    ),
    Req.

reply_success(Req0, Data) ->
    Req = cowboy_req:reply(
        200,
        #{<<"content-type">> => <<"application/json">>},
        Data,
        Req0
    ),
    Req.

init(Req0, State) ->
    GetBodyResult = get_decoded_body(Req0),
    case GetBodyResult of
        {ok, DecodedReqBody} ->
            ParseValidateExecMachineResult = parse_validate_and_execute(
                maps:get(list_to_binary("machineConfig"), DecodedReqBody),
                maps:get(list_to_binary("input"), DecodedReqBody)
            ),
            case ParseValidateExecMachineResult of
                {ok, Data} ->
                    Req = reply_success(Req0, Data),
                    {ok, Req, State};
                {error, FormattedError} ->
                    Req = reply_error(Req0, FormattedError),
                    {ok, Req, State}
            end;
        {error, invalid_body} ->
            Req = reply_error(Req0, "Body is invalid"),
            {ok, Req, State}
    end.
