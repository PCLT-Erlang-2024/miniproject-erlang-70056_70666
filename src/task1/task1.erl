-module(task1).
-export([start/1, stop/0, belt_loader/1, loading_bay/2, truck_generator/0]).

% Start factory
start(NumBelts) ->
    % To release the shell
    spawn(fun() ->
        % Create loading bays
        LoadingBays = [spawn(?MODULE, loading_bay, [ID, self()]) || ID <- lists:seq(1, NumBelts)],

        % Create belts, passing the corresponding loading bay PID
        Belts = [spawn(?MODULE, belt_loader, [{ID, lists:nth(ID, LoadingBays)}]) || ID <- lists:seq(1, NumBelts)],

        % Start truck generator - register for loading bays to request
        TruckGenerator = spawn(?MODULE, truck_generator, []),
        register(truck_generator, TruckGenerator),
        
        receive
            stop ->
                % Send stop signal to all belts and loading bays
                lists:foreach(fun(Pid) -> Pid ! stop end, Belts),
                lists:foreach(fun(Pid) -> Pid ! stop end, LoadingBays),
                TruckGenerator ! stop,
                io:format("Factory stopped~n")
        end
    end),
    ok.

% Stop the system
stop() ->
    self() ! stop,
    ok.

% Belt loader: produces packages and send to the right loading bay
belt_loader({ID, LoadingBay}) ->
    loop_loader(ID, LoadingBay, 0).

loop_loader(ID, LoadingBay, PID) ->
    timer:sleep(2500),
    Size = 1,
    io:format("[PACKAGE LOADER ~p]: Package belt_~p_~p sent to loading bay~n", [ID, ID, PID+1]),
    LoadingBay ! {self(), package, ID, PID+1, Size},
    loop_loader(ID, LoadingBay, PID+1).

loading_bay(ID, Belt) ->
    io:format("Loading Bay ~p started~n", [ID]),
    loop_bay(ID, Belt, null, null).

% ID - of the belt
% Belts - maybe for something, not sure what
% Capacity - current of the Truck if there's one - otherwise null
% Package - pending to be loaded - when a switch occurs
loop_bay(ID, Belts, Capacity, Package) ->
    % If no truck is assigned yet, request one.
    CurrCapacity = case Capacity of
        null ->
            io:format("[LOADING BAY ~p]: No truck available, ~p is requesting one~n", [ID, self()]),
            truck_generator ! {self(), request_truck},
            receive
                {SenderID, Cap} -> 
                    io:format("[LOADING BAY ~p]: Received truck with ~p capacity from ~p~n",[ID, Cap, SenderID]),
                    loop_bay(ID, Belts, Cap, Package);
                stop ->
                    io:format("[LOADING BAY ~p]: stopping~n", [ID])
            end;
        Capacity when is_number(Capacity) ->
            io:format("[LOADING BAY ~p]: Truck with ~p capacity~n", [ID, Capacity]),
            Capacity
    end,

    case Package of
        null ->
            receive
                {_, package, BeltID, PID, Size} ->
                    if
                        CurrCapacity - Size < 0 ->
                            io:format("[LOADING BAY ~p]: Truck full - shipped~n", [ID]),
                            loop_bay(ID, Belts, null, {BeltID, PID, Size});
                        true ->
                            io:format("[LOADING BAY ~p]: Package loaded onto truck on belt | CurrentCapacity: ~p~n",[ID, CurrCapacity - Size]),
                            loop_bay(ID, Belts, CurrCapacity - Size, null)
                    end;
                stop ->
                    io:format("[LOADING BAY ~p]: Loading bay stopping~n", [ID])
            end;
        % Pending package
        {BeltID, PID, Size} ->
            if
                CurrCapacity - Size < 0 ->
                    io:format("[LOADING BAY ~p]: Truck shipped~n", [ID]),
                    loop_bay(ID, Belts, null, {BeltID, PID, Size});
                true ->
                    io:format("[LOADING BAY ~p]: Pending package loaded onto truck~n",[ID]),
                    loop_bay(ID, Belts, CurrCapacity - Size, null)
            end
    end.

% Sends trucks when needed.
truck_generator() ->
    receive
        {BeltPID, request_truck} -> 
            io:format("[TRUCK GENERATOR]: Sent requested truck to ~p~n", [BeltPID]),
            Capacity = 5,
            BeltPID ! {self(), Capacity},
            truck_generator();
        stop -> 
            io:format("[TRUCK GENERATOR]: Truck generator stopping")
        end.