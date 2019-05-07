-module(order_handler).

-export([start/1]).

-define(NUMBER_OF_FLOORS,4).
-define(MAX_COST, ?NUMBER_OF_FLOORS*2).

-record(hall_requests, {
                        floor,
                        direction}).

-record(cab_requests, {
                        floor,
                        should_stop}).

-record(elevator_states, {
                        name,
                        behavior,
                        floor,
                        next_order}).

%%% Start %%%
start(OrderHandlerListener) ->
    spawn(fun()-> init_order_handler(OrderHandlerListener) end).

%%% Manage orders %%%
init_order_handler(OrderHandlerListener) ->
    put(listener, OrderHandlerListener),
    loop(OrderHandlerListener).

loop(OrderHandlerListener) ->
    CabOrders = storage:read_cab_requests(),
    HallOrders = storage:read_hall_requests(),

    case CabOrders /= [] of
        true ->
            {CheapestCabCost, CheapestCab} = get_cheapest_cab_order(CabOrders);
        false ->
            CheapestCabCost = ?MAX_COST,
            CheapestCab = []
    end,

    case HallOrders /= [] of
        true ->
            {CheapestHallCost, CheapestHall} = get_cheapest_hall_order(HallOrders, OrderHandlerListener);
        false ->
            CheapestHallCost = ?MAX_COST,
            CheapestHall = []
    end,
    
    if
        ((CheapestHallCost == ?MAX_COST) and (CheapestCabCost == ?MAX_COST)) ->
            storage:write_elevator_behavior(node(),idle);
        (CheapestHallCost < CheapestCabCost) ->
            handle_hall_order(OrderHandlerListener, CheapestHall);
        (CheapestCabCost < CheapestHallCost) ->
            handle_cab_order(OrderHandlerListener, CheapestCab);
        (CheapestCabCost == CheapestHallCost) ->
            handle_cab_order(OrderHandlerListener, CheapestCab)
    end,

    loop(OrderHandlerListener).

handle_hall_order(OrderHandlerListener, CheapestHall) ->
    State = storage:read_elevator_state(node()),
    {CheapestFloor, CheapestButton} = get_floor_and_button_from_order(CheapestHall),

    CheapestOrder = [{CheapestFloor, CheapestButton}],
    ElevatorNextOrder = State#elevator_states.next_order,
    
    case ElevatorNextOrder /= CheapestOrder of
        true ->
            OrderHandlerListener ! {order, CheapestHall#hall_requests.floor},
            storage:write_elevator_next_floor(node(),CheapestHall#hall_requests.floor, CheapestButton);
        false ->
            do_nothing
    end,

    receive
        {order, finished} ->
            storage:remove_request(CheapestHall#hall_requests.floor, CheapestButton),
            storage:write_elevator_next_floor(node(),empty,empty)
    after 0 ->
        loop(OrderHandlerListener)
    end.

handle_cab_order(OrderHandlerListener, CheapestCab) ->
    State = storage:read_elevator_state(node()),
    ElevatorNextOrder = State#elevator_states.next_order,
    CheapestOrder = [{CheapestCab#cab_requests.floor, cab}],

    case ElevatorNextOrder /= CheapestOrder of
        true ->
            storage:write_elevator_next_floor(node(),CheapestCab#cab_requests.floor, cab),
            OrderHandlerListener ! {order, CheapestCab#cab_requests.floor};
        false ->
            do_nothing
    end,

    receive
        {order, finished} ->
            storage:remove_request(CheapestCab#cab_requests.floor, cab),
            storage:write_elevator_next_floor(node(),empty,empty)
    after 0 ->
        loop(OrderHandlerListener)
    end.

get_cheapest_hall_order(Orders, OrderHandlerListener) ->
    State = storage:read_elevator_state(node()),
    F = fun(Order) ->
        {cost(State#elevator_states.behavior,State#elevator_states.floor, Order#hall_requests.floor, Order#hall_requests.direction), Order}
    end,
    Costs = lists:map(F,Orders),
    {MinCost, MinCostOrder} = lists:min(Costs),

    %% Check if order taken by another elevator %%
    IsTaken = is_already_taken(MinCostOrder),
    case IsTaken of
        true ->
            RemainingOrders = lists:delete(MinCostOrder,Orders),
            case (RemainingOrders==[]) of
                true ->
                    loop(OrderHandlerListener);
                false ->
                    get_cheapest_hall_order(RemainingOrders, OrderHandlerListener)
            end;
        false ->
            %% Check if this elevator has the lowest cost %%
            IsLowestCost = is_lowest_cost(MinCost, MinCostOrder),
            case IsLowestCost of
                true ->      
                    {MinCost, MinCostOrder};
                false ->
                    loop(OrderHandlerListener)
            end
    end.

get_cheapest_cab_order(Orders) ->
    State = storage:read_elevator_state(node()),
    F = fun(Order) ->
        {cost(State#elevator_states.behavior, State#elevator_states.floor, Order#cab_requests.floor), Order}
    end,
    Costs = lists:map(F,Orders),
    {MinCost, MinCostOrder} = lists:min(Costs).

%% Hall order cost function %%
cost(ElevatorBehavior, ElevatorFloor, OrderFloor, OrderDirection) ->
    FloorDirection = find_floor_direction(ElevatorBehavior, ElevatorFloor, OrderFloor),
    OrderSameDirection = is_order_in_same_direction(OrderDirection,ElevatorBehavior),

    case ((FloorDirection == ElevatorBehavior) or (ElevatorBehavior==idle)) of
        true ->
            case OrderSameDirection of
                true ->
                    abs(ElevatorFloor-OrderFloor);
                false ->
                    abs(ElevatorFloor-OrderFloor) + ?NUMBER_OF_FLOORS
            end;
        false ->
            abs(ElevatorFloor-OrderFloor) + ?NUMBER_OF_FLOORS
    end.

%% Cab order cost function %%
cost(ElevatorBehavior, ElevatorFloor, OrderFloor) ->
    FloorDirection = find_floor_direction(ElevatorBehavior, ElevatorFloor, OrderFloor),
    case (FloorDirection == ElevatorBehavior) of
        true ->
            abs(ElevatorFloor-OrderFloor);
        false ->
            abs(ElevatorFloor-OrderFloor) + ?NUMBER_OF_FLOORS
    end.

%%% Compare orders between elevators %%%
is_already_taken(MinCostOrder) ->
    Nodes = nodes(),
    {CheapestFloor, CheapestButton} = get_floor_and_button_from_order(MinCostOrder),
    CheapestOrder = [{CheapestFloor, CheapestButton}],

    IsTaken = case (length(Nodes) > 0) of
                true ->       
                    check_nodes_if_order_taken(Nodes, CheapestOrder);
                false ->
                    false
            end,
    IsTaken.

check_nodes_if_order_taken(Nodes, CheapestOrder) ->
    [Node | RemainingNodes] = Nodes,
    State = storage:read_elevator_state(Node),
    case (CheapestOrder == State#elevator_states.next_order) of
        true ->
            true;
        false ->
            case (RemainingNodes==[]) of
                true ->
                    false;
                false ->
                    check_nodes_if_order_taken(RemainingNodes, CheapestOrder)
            end
    end.

is_lowest_cost(MinCost, MinCostOrder) ->
    Nodes = nodes(),

    IsLowest = case (length(Nodes) > 0) of
                    true ->
                        check_nodes_if_lowest_cost(Nodes, MinCost, MinCostOrder);
                    false ->
                        true
                end,
    IsLowest.

check_nodes_if_lowest_cost(Nodes, MinCost, MinCostOrder) ->
    [Node | RemainingNodes] = Nodes,

    State = storage:read_elevator_state(Node),
    NodeBehavior = State#elevator_states.behavior,
    NodeFloor = State#elevator_states.floor,

    OrderFloor = MinCostOrder#hall_requests.floor,
    OrderDirection = MinCostOrder#hall_requests.direction,

    NodeCost = cost(NodeBehavior, NodeFloor, OrderFloor, OrderDirection),
    case (NodeCost < MinCost) of
        true ->
            false;
        false ->
            case (RemainingNodes==[]) of
                true ->
                    true;
                false ->
                    check_nodes_if_lowest_cost(RemainingNodes, MinCost, MinCostOrder)
            end
    end.

%%% Helpers %%%
get_floor_and_button_from_order(Order) ->
    Floor = Order#hall_requests.floor,
    Button = case Order#hall_requests.direction of
                [true,false] ->
                    hall_up;
                [false,true] ->
                    hall_down;
                 [true,true] ->
                    State = storage:read_elevator_state(node()),
                    Behavior = State#elevator_states.behavior,
                    case Behavior of
                        up ->
                            hall_up;
                        down ->
                            hall_down;
                        _ ->
                            hall_down
                    end
            end,
    {Floor,Button}.

find_floor_direction(ElevatorBehavior, ElevatorFloor, OrderFloor) ->
    if
        (ElevatorFloor-OrderFloor) < 0 ->
            up;
        (ElevatorFloor-OrderFloor) > 0 ->
            down;
        (ElevatorFloor-OrderFloor) == 0 ->
            ElevatorBehavior
    end.

is_order_in_same_direction(OrderDirection, ElevatorBehavior) ->
    Direction = case OrderDirection of
                    [true,false] ->
                        up;
                    [false,true] ->
                        down;
                    [true,true] ->
                        both
                end,

    if
        (Direction == both) ->
            true;
        (Direction == ElevatorBehavior) ->
            true;
        (Direction /= ElevatorBehavior) ->
            false
    end.

%%% Module interface %%%
send_order_finished(OrderHandlerPid) -> OrderHandlerPid ! {order, finished}.