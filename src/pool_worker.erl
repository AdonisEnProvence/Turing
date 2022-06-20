-module(pool_worker).

-export([init_pool_worker/1]).

init_pool_worker(
    MasterWorker, {execute_machine, {ReqActorPid, {ParsedMachineConfig, ParsedInput}}}
) ->
    From = self(),

    spawn(fun() ->
        MachineExecutionResponse = execute_machine_handler:execute_machine(
            ParsedMachineConfig, ParsedInput
        ),
        From ! {finished, MachineExecutionResponse}
    end),

    receive
        {execute_machine, _} ->
            MasterWorker ! {unavailable_worker, self()};
        {finished, MachineExecutionResponse} ->
            MasterWorker ! {finished_machine_execution, {ReqActorPid, MachineExecutionResponse}},
            init_pool_worker(MasterWorker)
    end.

init_pool_worker(MasterWorker) ->
    receive
        {execute_machine, {ReqActorPid, MachineConfigToExecute}} ->
            MasterWorker ! {started_machine_execution, self()},
            init_pool_worker(MasterWorker, {execute_machine, {ReqActorPid, MachineConfigToExecute}})
    end.
