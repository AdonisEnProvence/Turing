-module(pool_worker_master).
-define(WORKER_NUMBER, 4).

-export([init_pool_worker_master/0]).

find_worker_for_machine_execution(0, 3, _WorkerPidList, _MachineConfigToExecute, ReqActorPid) ->
    ReqActorPid ! {error, could_not_find_available_actor};
find_worker_for_machine_execution(
    0, RetryCounter, WorkerPidList, MachineConfigToExecute, ReqActorPid
) ->
    receive
    after 1000 ->
        find_worker_for_machine_execution(
            ?WORKER_NUMBER, RetryCounter + 1, WorkerPidList, MachineConfigToExecute, ReqActorPid
        )
    end;
find_worker_for_machine_execution(
    WorkerIndex, RetryCounter, WorkerPidList, MachineConfigToExecute, ReqActorPid
) ->
    CurrentWorker = lists:nth(WorkerIndex, WorkerPidList),
    CurrentWorker ! {execute_machine, {ReqActorPid, MachineConfigToExecute}},

    receive
        {unavailable_worker, _Pid} ->
            find_worker_for_machine_execution(
                WorkerIndex - 1, RetryCounter, WorkerPidList, MachineConfigToExecute, ReqActorPid
            );
        {started_machine_execution, _Pid} ->
            {ok}
    end.

init_pool_worker_master() ->
    init_pool_worker_master(?WORKER_NUMBER, []).

init_pool_worker_master(0, WorkerPidList) ->
    io:format("LISTENING TO MESSAGES\n"),
    receive
        {coco} ->
            io:format("pong\n");
        {execute_machine_request, {ReqActorPid, MachineConfigToExecute}} ->
            io:format("received execute_machine_request"),
            % below function should be async ?? spawn this ?
            find_worker_for_machine_execution(
                ?WORKER_NUMBER, 0, WorkerPidList, MachineConfigToExecute, ReqActorPid
            );
        {finished_machine_execution, {ReqActorPid, MachineExecutionResponse}} ->
            ReqActorPid ! {result, MachineExecutionResponse}
    end,
    io:format("Init pool worker master and workers finished ~p~n", [WorkerPidList]);
init_pool_worker_master(Index, WorkerPidList) ->
    PoolMasterWorkerPid = self(),
    NewWorkerPid = spawn(pool_worker, init_pool_worker, [PoolMasterWorkerPid]),
    init_pool_worker_master(Index - 1, [NewWorkerPid, WorkerPidList]).
