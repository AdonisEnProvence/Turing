-module(execute_machine_async_requester).

-export([request_async_execute_machine/2]).

request_async_execute_machine(PoolWorkerMasterPid, {ParsedMachineConfig, ParsedInput}) ->
    PoolWorkerMasterPid !
        {execute_machine_request, {self(), {ParsedMachineConfig, ParsedInput}}},
    receive
        {result, MachineExecutionResponse} ->
            MachineExecutionResponse;
        {error, could_not_find_available_actor} ->
            {error, could_not_find_available_actor}
    end.
