-module(events_stream).
-export([init/0, log_factory_start/0, log_factory_end/0]).
-export([log_package_created/2, log_package_loaded/3]).
-export([log_truck_created/1, log_truck_full/2, log_truck_replaced/2]).
-export([log_belt_stopped/1, log_belt_resumed/1]).
-export([get_statistics/0]).

-define(FACTORY_START, factory_start).
-define(FACTORY_END, factory_end).
-define(PACKAGE_CREATED, package_created).
-define(PACKAGE_LOADED, package_loaded).
-define(TRUCK_CREATED, truck_created).
-define(TRUCK_FULL, truck_full).
-define(TRUCK_REPLACED, truck_replaced).
-define(BELT_STOPPED, belt_stopped).
-define(BELT_RESUMED, belt_resumed).

get_statistics() ->
    Events = ets:tab2list(event_stream),
    [{total_packages, TotalPackages}] = ets:lookup(statistics, total_packages),
    [{total_trucks, TotalTrucks}] = ets:lookup(statistics, total_trucks),
    [{total_load, TotalLoad}] = ets:lookup(statistics, total_load),

    BeltStatistics = calculate_statistics(Events),

    StartTime = lists:min([Timestamp || {_, Timestamp, _} <- Events]),
    EndTime = lists:max([Timestamp || {_, Timestamp, _} <- Events]),
    TotalTime = (EndTime - StartTime) / 1000.0,

    io:format("~n=== Factory Operation Statistics ===~n"),
    io:format("Total Operation Time: ~.2f seconds~n", [TotalTime]),
    io:format("Total Packages Processed: ~p~n", [TotalPackages]),
    io:format("Total Trucks Used: ~p~n", [TotalTrucks]),
    io:format("Total Load Processed: ~p units~n", [TotalLoad]),
    io:format("Average Load per Truck: ~.2f units~n", [TotalLoad/TotalTrucks]),
    io:format("Packages per Second: ~.2f~n", [TotalPackages/TotalTime]),

    io:format("~nPer Belt Statistics:~n"),
    maps:foreach(
        fun(BeltNum, #{packages := PackageCount, load := LoadSize}) ->
            io:format("Belt ~p: ~p packages, ~p total load~n", 
                     [BeltNum, PackageCount, LoadSize])
        end, BeltStatistics),
    ok.

calculate_statistics(Events) -> 
    lists:foldl(fun({EventType, _Timestamp, Data}, Acc) ->
        case EventType of
            ?PACKAGE_CREATED ->
                BeltNum = maps:get(belt_num, Data),
                BeltStats = maps:get(BeltNum, Acc, #{packages => 0, load => 0}),
                PackageCount = maps:get(packages, BeltStats) + 1,
                LoadSize = maps:get(size, maps:get(package, Data)),
                TotalLoad = maps:get(load, BeltStats) + LoadSize,
                Acc#{BeltNum => BeltStats#{packages => PackageCount, 
                                         load => TotalLoad}};
            _ -> Acc 
        end
    end, #{}, Events).

log_factory_start() -> 
    io:format("[Factory] Starting Factory~n"),
    add_event(?FACTORY_START, erlang:system_time(millisecond)).

log_factory_end() ->    
    io:format("[Factory] End Operation~n"),
    add_event(?FACTORY_END, erlang:system_time(millisecond)).

log_package_created(BeltNum, Package) ->
    io:format("[Factory] Package Created with Size: ~p for Belt: ~p~n", 
              [maps:get(size, Package), BeltNum]),
    add_event(?PACKAGE_CREATED, #{belt_num => BeltNum, package => Package}),
    [{total_packages, TotalPackages}] = ets:lookup(statistics, total_packages),
    ets:insert(statistics, {total_packages, TotalPackages + 1}).

log_package_loaded(TruckId, Package, CurrentLoad) ->
    io:format("[Truck Operation] Loaded package ~p, current load: ~p/10~n",
              [maps:get(id, Package), CurrentLoad]),
    add_event(?PACKAGE_LOADED, #{truck_id => TruckId, package => Package, current_load => CurrentLoad}),
    [{total_load, TotalLoad}] = ets:lookup(statistics, total_load),
    ets:insert(statistics, {total_load, TotalLoad + maps:get(size, Package)}).

log_truck_created(TruckData) ->
    io:format("[Factory] Truck Created: ~p~n", [TruckData]),
    add_event(?TRUCK_CREATED, TruckData),
    [{total_trucks, TotalTrucks}] = ets:lookup(statistics, total_trucks),
    ets:insert(statistics, {total_trucks, TotalTrucks + 1}).

log_truck_full(TruckId, BeltNum) ->
    io:format("[Truck Operation] Truck ~p on belt ~p is full~n", [TruckId, BeltNum]),
    add_event(?TRUCK_FULL, #{truck_id => TruckId, belt_num => BeltNum}).

log_truck_replaced(BeltNum, NewTruckData) ->
    io:format("[Fleet Manager] New truck allocated for belt ~p: ~p~n", 
              [BeltNum, NewTruckData]),
    add_event(?TRUCK_REPLACED, #{belt_num => BeltNum, new_truck => NewTruckData}).

log_belt_stopped(BeltNum) ->
    io:format("[Belt Operation - ~p] Belt operation stopped~n", [BeltNum]),
    add_event(?BELT_STOPPED, #{belt_num => BeltNum}).

log_belt_resumed(BeltNum) ->
    io:format("[Belt Operation - ~p] Belt operation resumed~n", [BeltNum]),
    add_event(?BELT_RESUMED, #{belt_num => BeltNum}).

init() -> 
    ets:new(event_stream, [named_table, bag, public]),
    ets:new(statistics, [named_table, public]),
    ets:insert(statistics, {total_packages, 0}),
    ets:insert(statistics, {total_trucks, 0}),
    ets:insert(statistics, {total_load, 0}),
    ok.

add_event(EventType, Data) ->
    case ets:info(event_stream) of
        undefined ->
            init(),
            add_event(EventType, Data);
        _ ->
            Timestamp = erlang:system_time(millisecond),
            ets:insert(event_stream, {EventType, Timestamp, Data})
    end.
