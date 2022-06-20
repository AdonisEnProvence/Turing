-module(execute_machine_handler).
-behaviour(cowboy_handler).
-export([init/2, tape_history_accumulator_process/1]).

-include("machine.hrl").

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
            MachineConfigIsKey = maps:is_key(<<"machineConfig">>, DecodedReqBody),
            InputIsKey = maps:is_key(<<"input">>, DecodedReqBody),
            MachineConfigAndInputAreKeys = InputIsKey and MachineConfigIsKey,
            if
                MachineConfigAndInputAreKeys =:= true ->
                    {ok, DecodedReqBody};
                MachineConfigAndInputAreKeys =:= false ->
                    {error, invalid_body}
            end;
        {error, _Error} ->
            {error, invalid_body}
    end.

tape_history_accumulator_process(From, Accumulator) ->
    receive
        {push, ElementToPush} ->
            tape_history_accumulator_process(From, [ElementToPush | Accumulator]);
        get ->
            From ! {result, lists:reverse(Accumulator)}
    end.
tape_history_accumulator_process(From) ->
    tape_history_accumulator_process(From, []).

execute_machine(ParsedMachineConfig, ParsedInput) ->
    From = self(),
    AccumulatorPid = spawn(?MODULE, tape_history_accumulator_process, [From]),

    spawn(fun() ->
        interpreter:start(ParsedMachineConfig, ParsedInput, fun(
            {Tape, CurrentState, IndexOnTape, Status, _Transition}
        ) ->
            AccumulatorPid !
                {push, #tape_history_element{
                    tape = Tape,
                    currentState = CurrentState,
                    indexOnTape = IndexOnTape - 1,
                    status = Status
                }}
        end),
        From ! finished
    end),

    receive
        finished ->
            AccumulatorPid ! get,
            receive
                {result, Result} ->
                    ResponseBodyRecord = #execute_machine_response{
                        blank = ParsedMachineConfig#parsed_machine_config.blank,
                        tapeHistory = Result
                    },
                    {ok, ResponseBodyRecord}
            end
    after 3000 ->
        {error, halting_issue}
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

encode_reply_error_response_body(Message) ->
    jsone:try_encode(#{
        <<"reason">> => list_to_binary(Message)
    }).

reply_error(Req0, Message) ->
    EncodedResult = encode_reply_error_response_body(Message),
    case EncodedResult of
        {ok, EncodedResponseBody} ->
            Req = cowboy_req:reply(
                400,
                #{<<"content-type">> => <<"application/json">>},
                EncodedResponseBody,
                Req0
            ),
            Req;
        {error, _Error} ->
            Req = cowboy_req:reply(
                500,
                Req0
            ),
            Req
    end.

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
                    EncodedResult = encode_response_body(ResponseBodyRecord),
                    case EncodedResult of
                        {ok, EncodedResponseBody} ->
                            Req = reply_success(Req0, EncodedResponseBody),
                            {ok, Req, State};
                        {error, _Error} ->
                            Req = reply_error(Req0, "Response encode failure"),
                            {ok, Req, State}
                    end;
                {error, halting_issue} ->
                    Req = reply_error(Req0, "Machine too long to be executed, autokill.\n"),
                    {ok, Req, State};
                {error, FormattedError} ->
                    Req = reply_error(Req0, FormattedError),
                    {ok, Req, State}
            end;
        {error, invalid_body} ->
            Req = reply_error(Req0, "Body is invalid"),
            {ok, Req, State}
    end.
