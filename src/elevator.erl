-module(elevator).
-export( [start/2,
          send_floor/2]).

-define(NUMBER_OF_FLOORS, 4).
-define(SLEEP_PERIOD, 100).

%%% Start %%%
start(DriverPid, ElevatorListener) ->
  FloorSensePid = spawn(fun() -> init_floor_sensing(DriverPid, ElevatorListener) end),
  ButtonSensePid = spawn(fun() -> init_button_sensing(DriverPid, ElevatorListener) end),
  spawn(fun() -> start_floor_request_handler(ElevatorListener) end).


%%% Handle floor requests %%%
start_floor_request_handler(ElevatorListener) ->
  {InitialOrderFloor, OrderHandlerPid} = receive_order_floor(),
  InitialState = idle,
  floor_request_loop(ElevatorListener, InitialOrderFloor, OrderHandlerPid, InitialState).

floor_request_loop(ElevatorListener, PrevOrderFloor, OrderHandlerPid, PrevState) ->
  OrderFloor = receive_order_floor_interupt(PrevOrderFloor),
  CurrentFloor = receive_current_floor(),
  Difference = OrderFloor - CurrentFloor,
  if
    (Difference > 0) ->
      State = up;
    (Difference < 0) ->
      State = down;
    (Difference == 0) ->
      State = idle
  end,
  case ((State /= PrevState) or (State == idle)) of
    true ->
      case State of
        idle ->
          ElevatorListener ! {state, State},
          start_floor_request_handler(ElevatorListener);
        up ->
          ElevatorListener ! {state, State},
          floor_request_loop(ElevatorListener, OrderFloor, OrderHandlerPid, State);
        down ->
          ElevatorListener ! {state, State},
          floor_request_loop(ElevatorListener, OrderFloor, OrderHandlerPid, State)
      end;
    false ->
      floor_request_loop(ElevatorListener, OrderFloor, OrderHandlerPid, State)
  end.

receive_order_floor() ->
  receive
    {order, Floor, OrderHandlerPid} ->
      {Floor, OrderHandlerPid}
  end.

receive_order_floor_interupt(PrevOrderFloor) ->
  receive
    {order, Floor, OrderHandlerPid} ->
      Floor
  after 0 ->
    PrevOrderFloor
  end.

receive_current_floor() ->
  request_current_floor(self()),
  receive
    {floor, Floor} ->
      Floor
  end.

request_current_floor(Pid) ->
  Floor = elevator_interface:get_floor_sensor_state(driver),
  case Floor /= between_floors of
    true ->
      Pid ! {floor, Floor};
    false ->
      request_current_floor(Pid)
  end.

%%% Elevator floor sensing %%%
init_floor_sensing(DriverPid, ElevatorListener) ->
  InitialFloor = -1,
  floor_sensing(DriverPid, ElevatorListener, InitialFloor).

floor_sensing(DriverPid, ElevatorListener, PrevFloor) ->
  Floor = elevator_interface:get_floor_sensor_state(DriverPid),
  case Floor of
    PrevFloor ->
      continue;
    between_floors ->
      continue;
    _ ->
      ElevatorListener ! {new_floor, Floor}
  end,
  floor_sensing(DriverPid, ElevatorListener, Floor).

%%% Elevator button sensing %%%
init_button_sensing(DriverPid, ElevatorListener) ->
  start_main_loop(DriverPid, ElevatorListener).

start_main_loop(DriverPid, ElevatorListener) ->
  Floors = lists:seq(0, ?NUMBER_OF_FLOORS-1),
  loop_floors(DriverPid, ElevatorListener, Floors).

loop_floors(DriverPid, ElevatorListener, Floors) ->
  [Floor | RemainingFloors] = Floors,
  Buttons = [hall_up, hall_down, cab],
  loop_buttons_on_floor(DriverPid, ElevatorListener, Floor, Buttons),
  
  case RemainingFloors == [] of
    true ->
      start_main_loop(DriverPid, ElevatorListener);
    false ->
      loop_floors(DriverPid, ElevatorListener, RemainingFloors)
  end.

loop_buttons_on_floor(DriverPid, ElevatorListener, Floor, Buttons) ->
  [Button | RemainingButtons] = Buttons,
  State = check_button_state(DriverPid, Button, Floor),

  case State of
    1 ->
      ElevatorListener ! {order, {Floor, Button}};
    0 ->
      do_nothing
  end,

  set_button_light(DriverPid, ElevatorListener, Floor, Button),

  case RemainingButtons == [] of
    true ->
      continue;
    false ->
      loop_buttons_on_floor(DriverPid, ElevatorListener, Floor, RemainingButtons)
  end.

check_button_state(DriverPid, Button, Floor) ->
  if
    Floor == 0 ->
      case (Button == hall_up) or (Button == cab) of
        true ->
          State = elevator_interface:get_order_button_state(DriverPid, Floor, Button);
        false ->
          State = 0
      end;
    Floor == (?NUMBER_OF_FLOORS - 1) ->
      case (Button == hall_down) or (Button == cab) of
        true ->
          State = elevator_interface:get_order_button_state(DriverPid, Floor, Button);
        false ->
          State = 0
      end;
    (Floor > 0) and (Floor < (?NUMBER_OF_FLOORS - 1)) ->
      State = elevator_interface:get_order_button_state(DriverPid, Floor, Button)
  end,
  State.     

set_button_light(DriverPid, ElevatorListener, Floor, Button) ->
  ElevatorListener ! {isorder, {Floor, Button, self()}},
  receive
    {isorder, State} ->
      elevator_interface:set_order_button_light(DriverPid, Button, Floor, State)
  end.

%%% Module interface %%%
send_floor(ElevatorPid, Floor) -> ElevatorPid ! {order, Floor}.