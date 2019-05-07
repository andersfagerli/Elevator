-module(main).
-export([start/1]).

%%% Start %%%
start(ElevatorNumber) ->
  network:start(),
  storage:start(ElevatorNumber),

  {ok, DriverPid} = elevator_interface:start(),
  register(driver, DriverPid),

  FsmListener = spawn(fun() -> fsm_listener() end),
  FsmPid = fsm:start(FsmListener),
  register(fsm, FsmPid),

  ElevatorListener = spawn(fun()-> elevator_listener() end),
  ElevatorPid = elevator:start(DriverPid, ElevatorListener),
  register(elevator, ElevatorPid),

  OrderHandlerListener = spawn(fun()-> order_handler_listener() end),
  OrderHandlerPid = order_handler:start(OrderHandlerListener),
  register(order_handler, OrderHandlerPid).

%%% Listeners %%%
fsm_listener() ->
  receive
    init ->
      elevator_interface:set_motor_direction(driver, down),
      storage:write_elevator_behavior(node(), down);
    {drive, stop} ->
      elevator_interface:set_motor_direction(driver, stop);
    {drive, up} ->
      elevator_interface:set_motor_direction(driver, up),
      storage:write_elevator_behavior(node(), up);
    {drive, down} ->
      elevator_interface:set_motor_direction(driver, down),
      storage:write_elevator_behavior(node(), down);
    {door, open} ->
      elevator_interface:set_door_open_light(driver, on);
    {door, close} ->
      elevator_interface:set_door_open_light(driver, off)
  end,
  fsm_listener().

elevator_listener() ->
  receive
    {order, {Floor, ButtonType}} ->
      storage:write_order_request(Floor, ButtonType);
    {new_floor, Floor} ->
      elevator_interface:set_floor_indicator(driver, Floor),
      storage:write_elevator_floor(node(), Floor),
      fsm:send_new_floor(fsm, Floor);
    {state, idle} ->
      fsm:send_order_finished(fsm),
      fsm:send_door_open(fsm),
      order_handler:send_order_finished(order_handler);
    {state, Direction} ->
      fsm:send_order_direction(fsm, Direction);
    {isorder, {Floor, Button, ButtonSensePid}} ->
      State = storage:is_order(Floor, Button),
      ButtonSensePid ! {isorder, State}
  end,
  elevator_listener().

order_handler_listener() ->
  receive
    {order, Floor} ->
      elevator:send_floor(elevator, Floor)
  end,
  order_handler_listener().
