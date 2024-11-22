-module(task1).
-export([start/0, fleet_manager/1, start_operation/2, new_truck/2, truck_operation/1, belt_operation/2,  package_generator/2, allocate_truck/3]).


new_truck(OperationNumber, FleetManagerPID) -> #{
           id => OperationNumber,
           capacity => 5,
           current_load => 0,
           belt_num => OperationNumber,
           manager_pid => FleetManagerPID 
       }.

allocate_truck(FleetState, BeltNumber, NewTruck) ->
    TruckPid = spawn(task1, truck_operation, [NewTruck]),
    BeltPids = maps:get(belts_pids, FleetState, #{}),
    case maps:find(BeltNumber, BeltPids) of
        {ok, BeltPID} -> BeltPID ! {new_truck, TruckPid};
        error -> ok
    end.

fleet_manager(FleetState) -> 
    io:format("[Fleet Manager] Run ~p~n", [FleetState]),
    receive
        {conveyor_belt, BeltNumber, BeltPID} -> 
            io:format("[Fleet Manager] Received conveyor belt ~p~n", [BeltNumber]),
            fleet_manager(FleetState#{belts_pids => maps:put(BeltNumber, BeltPID, maps:get(belts_pids, FleetState, #{}))});
        {truck_full, BeltNumber} -> 
            io:format("[Fleet Manager] Truck full for belt ~p~n", [BeltNumber]),
            NewTruck = new_truck(BeltNumber, self()),
            allocate_truck(FleetState, BeltNumber, NewTruck),
            io:format("[Fleet Manager] Allocated Truck ~p~n", [NewTruck]),
            fleet_manager(FleetState)
   end.

truck_operation(Truck) -> 
    io:format("[Truck Operation] Truck: ~p~n", [Truck]),
    receive
        {load_package, Package, BeltPID} -> 
            io:format("[Truck Operation] Received package ~p~n", [Package]),
            TruckCapacity = maps:get(capacity, Truck),
            NewCurrentLoad = maps:get(current_load, Truck) + maps:get(size, Package),
            if 
                NewCurrentLoad =< TruckCapacity ->
                    io:format("[Truck Operation] Loaded package ~p, current load: ~p/~p~n",
                              [maps:get(id, Package), NewCurrentLoad, maps:get(capacity, Truck)]),
                    truck_operation(Truck#{current_load := NewCurrentLoad});
                true ->
                    io:format("[Truck Operation] Full! Requesting replacement~n"),
                    TruckManagerPID = maps:get(manager_pid, Truck),
                    TruckManagerPID ! {truck_full, maps:get(belt_num, Truck)},
                    BeltPID ! {truck_full, self()},
                    ok
            end;
        shutdown -> ok
    end.


belt_operation(ActiveTruckPID, OperationNumber) -> 
    io:format("[Belt Operation - ~p] Belt running with Truck PID: ~p~n", [OperationNumber, ActiveTruckPID]),
    receive
        {package, Package} -> 
            io:format("[Belt Operation - ~p] Received package ~p~n", [OperationNumber, Package]),
            ActiveTruckPID ! {load_package, Package, self()},
            belt_operation(ActiveTruckPID, OperationNumber);
        {new_truck, NewTruckPid} -> 
            io:format("[Belt Operation - ~p] New Truck ~p~n", [OperationNumber, NewTruckPid]),
            belt_operation(NewTruckPid, OperationNumber);
        {truck_full, TruckPID} when TruckPID =:= ActiveTruckPID ->
            io:format("[Belt Operation - ~p] Waiting for new truck...~n", [OperationNumber]),
            receive
                {new_truck, NewTruckPid} ->
                    io:format("[Belt Operation - ~p] Received new Truck ~p~n", [OperationNumber, NewTruckPid]),
                    belt_operation(NewTruckPid, OperationNumber)
            end

    end.

package_generator(BeltPID, BeltNum) ->
    io:format("[Task 1 - Package Generator] New Package for Belt: ~p~n", [BeltNum]),
    Package = #{id => lists:flatten(io_lib:format("Belt~p-Package~p", [BeltNum, rand:uniform(1000)])), 
                size => 1},
    BeltPID ! {package, Package},
    timer:sleep(rand:uniform(1000)),
    package_generator(BeltPID, BeltNum).


start_operation(OperationNumber, FleetManagerPID) ->
    io:format("[Conveyor Operation] Starting Operation ~p~n", [OperationNumber]),
    ActiveTruckPID = spawn(task1, truck_operation, [new_truck(OperationNumber, FleetManagerPID)]),
    io:format("[Conveyor Operation] Started Truck Operation ~p~n", [ActiveTruckPID]),
    ActiveBeltPID = spawn(task1, belt_operation, [ActiveTruckPID, OperationNumber]),
    io:format("[Conveyor Operation] Started Belt Operation ~p~n", [ActiveBeltPID]),
    FleetManagerPID ! {conveyor_belt, OperationNumber, ActiveBeltPID},
    io:format("[Conveyor Operation] Belt Operation Set~n"),
    PackageGeneratorPID = spawn(task1, package_generator, [ActiveBeltPID, OperationNumber]),
    io:format("[Conveyor Operation] Package Generator Set ~p~n", [PackageGeneratorPID]),
    [ActiveTruckPID, ActiveBeltPID, PackageGeneratorPID].


start() -> 
    io:format("[Factory] Starting Factory~n"),
    timer:sleep(1000),

    FleetManagerPID = spawn(task1, fleet_manager, [#{trucks => #{}, belts_pids => #{}}]),
    io:format("[Factory] Spawned Fleet Manager~p~n", [FleetManagerPID]),
    timer:sleep(1000),

    Operation = [start_operation(N, FleetManagerPID) || N <- lists:seq(1, 3)],

    % Factory Operation Time
    timer:sleep(10000),

    exit(FleetManagerPID, kill),
    lists:foreach(fun(Pids) ->
        lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids)
    end, Operation),

    io:format("[Factory] Finishing Fleet Manager~n"),
    io:format("[Factory] End Operation~n").
