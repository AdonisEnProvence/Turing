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
tape_history_accumulator_process(From, Accumulator) ->
    receive
        {push, ElementToPush} ->
            tape_history_accumulator_process(From, Accumulator ++ ElementToPush);
        get ->
            From ! {result, Accumulator}
    end.
tape_history_accumulator_process(From) ->
    tape_history_accumulator_process(From, []).

execute_machine(ParsedMachineConfig, ParsedInput) ->
    From = self(),
    AccumulatorPid = spawn(?MODULE, tape_history_accumulator_process, [From]),

    interpreter:start(ParsedMachineConfig, ParsedInput, fun(
        {Tape, CurrentState, IndexOnTape, Status, _Transition}
    ) ->
        AccumulatorPid !
            {push, [
                #tape_history_element{
                    tape = Tape,
                    currentState = CurrentState,
                    indexOnTape = IndexOnTape,
                    status = Status
                }
            ]}
    end),

    AccumulatorPid ! get,
    receive
        {result, Result} ->
            ResponseBodyRecord = #execute_machine_response{
                blank = ParsedMachineConfig#parsed_machine_config.blank,
                tapeHistory = Result
            },
            {ok, ResponseBodyRecord}
    end.

parse_validate_and_execute(RawMachineConfig, RawInput) ->
    StringInput = binary_to_list(RawInput),
    ParserValidatorResult = parse_and_validate_machine_config:parse_and_validate_decoded_machine_and_input(
        RawMachineConfig, StringInput
    ),

    case ParserValidatorResult of
        {ok, ParsedMachineConfig, ParsedInput} ->
            execute_machine(ParsedMachineConfig, ParsedInput);
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

encode_response_body(ResponseBodyRecord) ->
    TapeHistoryRecordList = ResponseBodyRecord#execute_machine_response.tapeHistory,
    TapeHistoryBitString = lists:map(
        fun(TapeHistoryElement) ->
            #{
                <<"tape">> => lists:map(
                    fun(TapeCharacter) -> list_to_binary(TapeCharacter) end,
                    TapeHistoryElement#tape_history_element.tape
                ),
                <<"currentState">> => list_to_binary(
                    TapeHistoryElement#tape_history_element.currentState
                ),
                <<"indexOnTape">> =>
                    TapeHistoryElement#tape_history_element.indexOnTape,

                <<"status">> => list_to_binary(
                    atom_to_list(TapeHistoryElement#tape_history_element.status)
                )
            }
        end,
        TapeHistoryRecordList
    ),
    ResponseBodyBitstring = #{
        <<"blank">> => list_to_binary(ResponseBodyRecord#execute_machine_response.blank),
        <<"tapeHistory">> => TapeHistoryBitString
    },
    jsone:try_encode(ResponseBodyBitstring).

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
                {ok, ResponseBodyRecord} ->
                    EncodeResult = encode_response_body(ResponseBodyRecord),
                    case EncodeResult of
                        {ok, EncodedResponseBody} ->
                            Req = reply_success(Req0, EncodedResponseBody),
                            {ok, Req, State};
                        {error, _Error} ->
                            Req = reply_error(Req0, "Response encode failure"),
                            {ok, Req, State}
                    end;
                {error, FormattedError} ->
                    Req = reply_error(Req0, FormattedError),
                    {ok, Req, State}
            end;
        {error, invalid_body} ->
            Req = reply_error(Req0, "Body is invalid"),
            {ok, Req, State}
    end.
