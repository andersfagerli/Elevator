-module(storage).
-include_lib ("mnesia/src/mnesia.hrl").

-export([
        start/1,
        read_cab_requests/0,
        read_hall_requests/0,
        read_elevator_state/1,
        read_elevator_floor/1,
        write_elevator_behavior/2,
        write_elevator_floor/2,
        write_elevator_next_floor/3,
        write_order_request/2,
        remove_request/2,
        is_order/2
        ]).

-define(NUMBER_OF_FLOORS, 4).
-define(NUMBER_OF_ELEVATORS, 3).
-define(STORAGE_INIT_TIME, 10000).

-record(hall_requests, {
                        floor,          %% 0,1,..,NUMBER_OF_FLOORS-1 %%
                        direction}).    %% [true,false] -> [up,down] %%

-record(cab_requests, {
                        floor,          %% 0,1,..,NUMBER_OF_FLOORS-1 %%
                        should_stop}).  %%         true/false        %%

-record(elevator_states, {
                        name,           %%           node()          %%
                        behavior,       %%        idle/up/down       %%
                        floor,          %% 0,1,..,NUMBER_OF_FLOORS-1 %%
                        next_order}).   %%     [{button, floor}]     %%

%%% Start %%%
start(ElevatorNumber) ->
    storage(ElevatorNumber).

%%% Initialize database %%%
storage(ElevatorNumber) ->
    wait_for_final_connection(),
    case (ElevatorNumber == ?NUMBER_OF_ELEVATORS) of
        true ->
            Nodes = [node()|nodes()],
            install(Nodes),
            init_elevator_state(),
            init_hall_requests();
        false ->
            mnesia:start(),
            timer:sleep(?STORAGE_INIT_TIME),
            init_elevator_state()
    end.

install(Nodes) ->
    mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),

    mnesia:create_table(hall_requests, [{record_name, hall_requests},
                                        {attributes, record_info(fields, hall_requests)},
                                        {disc_copies, Nodes}
                                        ]),

    mnesia:create_table(elevator_states, [{record_name, elevator_states},
                                          {attributes, record_info(fields, elevator_states)},
                                          {disc_copies, Nodes}
                                          ]),

    mnesia:create_table(cab_requests, [{record_name, cab_requests},
                                       {attributes, record_info(fields, cab_requests)},
                                       {disc_copies, Nodes},
                                       {local_content, true}
                                       ]),

    mnesia:wait_for_tables([hall_requests, cab_requests, elevator_states], ?STORAGE_INIT_TIME).

init_elevator_state() ->
    write_elevator_states(node(), idle, 0, [empty,empty]).

init_hall_requests() ->
    Floors = lists:seq(0,?NUMBER_OF_FLOORS-1),
    fill_database_hall_requests(Floors).

fill_database_hall_requests(Floors) ->
    [Floor | RemainingFloors] = Floors,
    F = fun() ->
            mnesia:write(hall_requests, #hall_requests{floor=Floor, direction=[false,false]}, write)
        end,
    mnesia:transaction(F),
    case RemainingFloors == [] of
        true ->
            init_hall_requests_complete;
        false ->
            fill_database_hall_requests(RemainingFloors)
    end.

wait_for_final_connection() ->
    Nodes = [node()|nodes()],
    case (length(Nodes) == ?NUMBER_OF_ELEVATORS) of
        true ->
            all_nodes_connected;
        false ->
            wait_for_final_connection()
    end.

%%% Module interface %%%
write_elevator_behavior(Name, Behavior) ->
    F = fun() ->
        [P] = mnesia:wread({elevator_states, Name}),
        mnesia:write(P#elevator_states{behavior = Behavior})
    end,
    mnesia:transaction(F).

write_elevator_floor(Name, Floor) ->
    F = fun() ->
        [P] = mnesia:wread({elevator_states, Name}),
        mnesia:write(P#elevator_states{floor = Floor})
    end,
    mnesia:transaction(F).

write_elevator_next_floor(Name, Floor, Button) ->
    F = fun() ->
        [P] = mnesia:wread({elevator_states, Name}),
        mnesia:write(P#elevator_states{next_order = [{Floor,Button}]})
    end,
    mnesia:transaction(F).

read_cab_requests() ->
    F = fun() ->
        mnesia:match_object(cab_requests, #cab_requests{floor='_', should_stop='_'}, read)
    end,
    {_, Orders} = mnesia:transaction(F),
    Orders.

read_hall_requests() ->
    HallUpF = fun() ->
        mnesia:match_object(hall_requests, #hall_requests{floor='_', direction=[true,'_']}, read)
    end,
    {_, HallUpOrders} = mnesia:transaction(HallUpF),

    HallDownF = fun() ->
        mnesia:match_object(hall_requests, #hall_requests{floor='_', direction=['_',true]}, read)
    end,
    {_, HallDownOrders} = mnesia:transaction(HallDownF),

    Orders = lists:merge(HallUpOrders,HallDownOrders),
    lists:usort(Orders).

read_elevator_state(Name) ->
    F = fun() ->
        mnesia:match_object(elevator_states, #elevator_states{name=Name, behavior='_', floor='_', next_order='_'}, read)
    end,
    {_, States} = mnesia:transaction(F),
    [State] = States,
    State.

read_elevator_floor(Name) ->
    State = read_elevator_state(Name),
    Floor = State#elevator_states.floor,
    Floor.

remove_request(Floor, Direction) ->
    case Direction of
        hall_up ->
            remove_request_hall(Floor, up);
        hall_down ->
            remove_request_hall(Floor, down);
        cab ->
            remove_request_cab(Floor)
    end.


is_order(Floor, Button) ->
    case Button of
        hall_up ->
            F = fun() ->
                mnesia:match_object(hall_requests, #hall_requests{floor=Floor, direction=[true,'_']}, read)
            end,
            {_, Orders} = mnesia:transaction(F),
            if
                length(Orders) == 1 ->
                    State = on;
                length(Orders) == 0 ->
                    State = off
            end;
        hall_down ->
            F = fun() ->
                mnesia:match_object(hall_requests, #hall_requests{floor=Floor, direction=['_',true]}, read)
            end,
            {_, Orders} = mnesia:transaction(F),
            if
                length(Orders) == 1 ->
                    State = on;
                length(Orders) == 0 ->
                    State = off
            end;
        cab ->
            F = fun() ->
                mnesia:match_object(cab_requests, #cab_requests{floor=Floor, should_stop=true}, read)
            end,
            {_, Orders} = mnesia:transaction(F),
            if
                length(Orders) == 1 ->
                    State = on;
                length(Orders) == 0 ->
                    State = off
            end
    end,
    State.

write_order_request(Floor, Button) ->
    case Button of
        hall_up ->
            write_hall_requests(Floor, up);
        hall_down ->
            write_hall_requests(Floor, down);
        cab ->
            write_cab_requests(Floor)
    end.

%%% Helpers %%%
write_hall_requests(Floor, Direction) ->
    Order = read_hall_requests(Floor),
    PrevDirection = Order#hall_requests.direction,
    [PrevHallUp | [PrevHallDown]] = PrevDirection,
    case Direction of
        up ->
            F = fun() ->
                mnesia:write(hall_requests, #hall_requests{floor=Floor, direction=[true,PrevHallDown]}, write)
            end;
        down ->
            F = fun() ->
                mnesia:write(hall_requests, #hall_requests{floor=Floor, direction=[PrevHallUp,true]}, write)
            end
    end,
    mnesia:transaction(F).

write_cab_requests(Floor) ->
    F = fun() ->
    mnesia:write(cab_requests, #cab_requests{floor=Floor,should_stop=true}, write)
    end,
    mnesia:transaction(F).

remove_request_hall(Floor, Direction) ->
    Order = read_hall_requests(Floor),
    PrevDirection = Order#hall_requests.direction,
    [PrevHallUp | [PrevHallDown]] = PrevDirection,

    case Direction of
        up ->
            F = fun() ->
                mnesia:write(hall_requests, #hall_requests{floor=Floor, direction=[false,PrevHallDown]}, write)
            end;
        down ->
            F = fun() ->
                mnesia:write(hall_requests, #hall_requests{floor=Floor, direction=[PrevHallUp,false]}, write)
            end
    end,
    mnesia:transaction(F).

remove_request_cab(Floor) ->
    F = fun() ->
        mnesia:delete_object(cab_requests, #cab_requests{floor=Floor, should_stop=true}, write)
    end,
    mnesia:transaction(F).

write_elevator_states(Name, Behavior, Floor, NextOrder) ->
    F = fun() ->
    mnesia:write(elevator_states, #elevator_states{name=Name, behavior=Behavior, floor=Floor, next_order=NextOrder}, write)
    end,
    mnesia:transaction(F).

read_hall_requests(Floor) ->
    F = fun() ->
    mnesia:read(hall_requests, Floor)
    end,
    {_, Orders} = mnesia:transaction(F),
    [Order] = Orders,
    Order.
