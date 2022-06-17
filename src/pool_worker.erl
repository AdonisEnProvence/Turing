-module(pool_worker).

-export([init_pool_worker/1]).

init_pool_worker(
    MasterWorker, {execute_machine, {ReqActorPid, {ParsedMachineConfig, ParsedInput}}}
) ->
    From = self(),

    spawn(fun() ->
        io:format("Start execution\n"),
        MachineExecutionResponse = execute_machine_handler:execute_machine(
            ParsedMachineConfig, ParsedInput
        ),
        io:format("End of machine execution ~p~n", [MachineExecutionResponse]),
        From ! {finished, MachineExecutionResponse}
    end),

    receive
        {execute_machine, _} ->
            io:format("POOL_WORKER RECEIVED = execute_machine BUT NOT AVAILABLE"),
            MasterWorker ! {unavailable_worker, self()};
        {finished, MachineExecutionResponse} ->
            io:format("POOL_WORKER RECEIVED = finished proxy to master worker"),
            MasterWorker ! {finished_machine_execution, {ReqActorPid, MachineExecutionResponse}},
            init_pool_worker(MasterWorker)
    end.

init_pool_worker(MasterWorker) ->
    receive
        {execute_machine, {ReqActorPid, MachineConfigToExecute}} ->
            io:format("POOL_WORKER RECEIVED = execute_machine AND AVAILABLE"),
            MasterWorker ! {started_machine_execution, self()},
            init_pool_worker(MasterWorker, {execute_machine, {ReqActorPid, MachineConfigToExecute}})
    end.
