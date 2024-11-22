-module(task3).
-export([start/0, fleet_manager/1, start_operation/2, new_truck/2, truck_operation/1, belt_operation/2,  package_generator/2, allocate_truck/3]).


new_truck(OperationNumber, FleetManagerPID) -> #{
                                                 id => OperationNumber,
                                                 capacity => 10,
                                                 current_load => 0,
                                                 belt_num => OperationNumber,
                                                 manager_pid => FleetManagerPID
                                                }.

allocate_truck(FleetState, BeltNumber, NewTruck) ->
    TruckPid = spawn(task3, truck_operation, [NewTruck]),
    events_stream:log_truck_created(NewTruck),
    BeltPids = maps:get(belts_pids, FleetState, #{}),
    case maps:find(BeltNumber, BeltPids) of
        {ok, BeltPID} -> BeltPID ! {new_truck, TruckPid};
        error -> ok
    end.

fleet_manager(FleetState) -> 
    receive
        {conveyor_belt, BeltNumber, BeltPID} -> 
            fleet_manager(FleetState#{belts_pids => maps:put(BeltNumber, BeltPID, maps:get(belts_pids, FleetState, #{}))});
        {truck_full, BeltNumber, BeltPID} -> 
            BeltPID ! {stop_belt_operation, BeltNumber},

            ReplacementTime = rand:uniform(2000),
            timer:sleep(ReplacementTime),

            NewTruck = new_truck(BeltNumber, self()),
            allocate_truck(FleetState, BeltNumber, NewTruck),
            events_stream:log_truck_replaced(BeltNumber, NewTruck),

            BeltPID ! {resume_belt_operation, BeltNumber},

            fleet_manager(FleetState)
    end.

truck_operation(Truck) -> 
    receive
        {load_package, Package, BeltPID} -> 
            TruckCapacity = maps:get(capacity, Truck),
            NewCurrentLoad = maps:get(current_load, Truck) + maps:get(size, Package),
            if 
                NewCurrentLoad =< TruckCapacity ->
                    events_stream:log_package_loaded(maps:get(id, Package), Package, maps:get(current_load, Truck)),
                    truck_operation(Truck#{current_load := NewCurrentLoad});
                true ->
                    events_stream:log_truck_full(maps:get(belt_num, Truck), BeltPID),
                    TruckManagerPID = maps:get(manager_pid, Truck),
                    TruckManagerPID ! {truck_full, maps:get(belt_num, Truck), BeltPID},
                    BeltPID ! {truck_full, self()},
                    ok
            end;
        shutdown -> ok
    end.


belt_operation(ActiveTruckPID, OperationNumber) -> 
    receive
        {stop_belt_operation} -> 
            events_stream:log_belt_stopped(OperationNumber),
            receive
                {resume_belt_operation} ->
                    events_stream:log_belt_resumed(OperationNumber),
                    belt_operation(ActiveTruckPID, OperationNumber)
            end;
        {package, Package} -> 
            ActiveTruckPID ! {load_package, Package, self()},
            belt_operation(ActiveTruckPID, OperationNumber);
        {truck_full, TruckPID} when TruckPID =:= ActiveTruckPID ->
            receive {new_truck, NewTruckPid} -> 
                belt_operation(NewTruckPid, OperationNumber)
            end
    end.

package_generator(BeltPID, BeltNum) ->
    Package = #{id => lists:flatten(io_lib:format("Belt~p-Package~p", [BeltNum, rand:uniform(1000)])), 
                size => rand:uniform(5)},
    events_stream:log_package_created(BeltNum, Package),
    BeltPID ! {package, Package},
    timer:sleep(rand:uniform(1000)),
    package_generator(BeltPID, BeltNum).


start_operation(OperationNumber, FleetManagerPID) ->
    ActiveTruckPID = spawn(task3, truck_operation, [new_truck(OperationNumber, FleetManagerPID)]),
    ActiveBeltPID = spawn(task3, belt_operation, [ActiveTruckPID, OperationNumber]),
    ActiveBeltPID ! {resume_belt_operation, OperationNumber},
    FleetManagerPID ! {conveyor_belt, OperationNumber, ActiveBeltPID},
    PackageGeneratorPID = spawn(task3, package_generator, [ActiveBeltPID, OperationNumber]),
    [ActiveTruckPID, ActiveBeltPID, PackageGeneratorPID].


start() -> 
    events_stream:init(),
    events_stream:log_factory_start(),
    timer:sleep(1000),

    FleetManagerPID = spawn(task3, fleet_manager, [#{trucks => #{}, belts_pids => #{}}]),
    timer:sleep(1000),

    Operation = [start_operation(N, FleetManagerPID) || N <- lists:seq(1, 3)],

    timer:sleep(10000),

    events_stream:get_statistics(),
    events_stream:log_factory_end(),

    lists:foreach(fun(Pids) ->
                          lists:foreach(fun(Pid) -> 
                                                exit(Pid, kill)
                                        end, Pids)
                  end, Operation),
    exit(FleetManagerPID, kill),
    ok.

