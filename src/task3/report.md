# Non-Instant Truck Replacement System: Correctness Analysis

## Overview
Our factory simulation implements a realistic truck replacement system where full trucks take time to be replaced, requiring conveyor belts to pause temporarily. Here's how we ensure everything works correctly.

## How to Run

```erlang
make shell
task3:start().
```

## Key Properties & Examples

### 1. Safe Belt Stopping
When a truck gets full, the belt must stop immediately to prevent package overflow. This is handled through a simple message exchange:

```erlang
% When truck is full, it notifies the belt
truck_operation(Truck) ->
    receive
        {load_package, Package, BeltPID} ->
            if NewCurrentLoad > TruckCapacity ->
                BeltPID ! {truck_full, self()},
                % ...
            end
    end.

% Belt stops and waits for resume signal
belt_operation(ActiveTruckPID, OperationNumber) -> 
    receive
        {stop_belt_operation} -> 
            % Belt is paused here until resumed
            receive
                {resume_belt_operation} ->
                    belt_operation(ActiveTruckPID, OperationNumber)
            end
    end.
```

### 2. Non-Instant Replacement
Truck replacement takes a random amount of time (0-2 seconds) to simulate real-world conditions:

```erlang
% In fleet_manager
{truck_full, BeltNumber, BeltPID} -> 
    BeltPID ! {stop_belt_operation, BeltNumber},
    % Simulate replacement time
    ReplacementTime = rand:uniform(2000),
    timer:sleep(ReplacementTime),
    % Create and allocate new truck
    NewTruck = new_truck(BeltNumber, self()),
    % ...
```

### 3. State Consistency 
The system uses message passing to keep track of which truck is active for each belt:

```erlang
% Belt switches to new truck when replacement happens
receive {new_truck, NewTruckPid} -> 
    belt_operation(NewTruckPid, OperationNumber)
end
```

## Verification
The system logs events to an event stream, which can be used for analysis and verification.
We can verify correct operation, which shows the sequence of events and generate a final statistics report.

```
[Truck Operation] Truck 1 on belt 1 is full
[Belt Operation - 1] Belt operation stopped
... (delay for replacement)
[Fleet Manager] New truck allocated for belt 1: #{...}
[Belt Operation - 1] Belt operation resumed
```

## Edge Cases Handled
1. Messages sent while belt is stopped are queued and processed later
2. Multiple belts can have trucks replaced concurrently
3. Package generator continues running but messages are buffered by the stopped belt

The system maintains correctness through careful message passing between components and explicit state management during transitions.
