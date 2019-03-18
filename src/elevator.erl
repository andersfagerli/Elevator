-module(elevator).
-export([start/0]).

-define(?SLEEP_PERIOD, 100).

start() ->
  % Do something
  {ok, DriverPid} = elevator_interface:start(),
  spawn(fun() -> init_floor_sensing(DriverPid) end),
  register(driver, DriverPid),

  FsmListener = spawn(fun() -> fsm_listener() end),
  FsmPid = fsm:start(FsmListener),
  register(fsm, FsmPid),
  ok.


%%% Listeners %%%
fsm_listener() ->
  receive
    init ->
      elevator_interface:set_motor_direction(driver, down);
    init_complete ->
      elevator_interface:set_motor_direction(driver, stop)
  end.

%%% Elevator interface %%%
init_floor_sensing(DriverPid) ->
  InitialFloor = elevator_interface:get_floor_sensor_state(DriverPid),
  floor_sensing(DriverPid, InitialFloor).

floor_sensing(DriverPid, PrevFloor) ->
  Floor = elevator_interface:get_floor_sensor_state(DriverPid),
  case Floor of
    PrevFloor ->
      timer:sleep(?SLEEP_PERIOD);
    _ ->
      fsm ! {at_floor, Floor} % at_floor may be "between_floors"
  end,
  floor_sensing(Floor).
