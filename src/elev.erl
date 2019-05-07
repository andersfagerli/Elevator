-module(elev).

-export([
  sens_to_mnesia/1,
  light_func/1,
  get_set_floor_sensor/1,
  get_set_floor_sensor/2,
  get_set_cab_buttons/1
]).


%Use only if on floor
%OrderList = [[false,false],[false,false],[false,false],[false,true]]

%changes true to on and false to off for set_order_button_light function
on_off(Boolean) ->
  case Boolean of
    true ->
      on;
    false ->
      off
  end.

%TODO make it so mnesia is used less often

%%%Reads from mnesia and sets lights. low priority maybe higher sleeps

light_func(DriverPid) ->
  {atomic,[[Up0,Down0], [Up1,Down1], [Up2,Down2], [Up3,Down3]]} = storage:select_hall_requests(),
  {atomic,[Cab0,Cab1,Cab2,Cab3]} = storage:select_cab_requests(),
  {atomic,[{elevator_states,_, _, _, Floor, _}]} = storage:read_elevator_states(node()),
  %%remember to fold if time
  elevator_interface:set_order_button_light(DriverPid, hall_up, 0, on_off(Up0)),
  elevator_interface:set_order_button_light(DriverPid, hall_up, 1, on_off(Up1)),
  elevator_interface:set_order_button_light(DriverPid, hall_up, 2, on_off(Up2)),
  elevator_interface:set_order_button_light(DriverPid, hall_up, 3, on_off(Up3)),
  
  elevator_interface:set_order_button_light(DriverPid, hall_down, 0, on_off(Down0)),
  elevator_interface:set_order_button_light(DriverPid, hall_down, 1, on_off(Down1)),
  elevator_interface:set_order_button_light(DriverPid, hall_down, 2, on_off(Down2)),
  elevator_interface:set_order_button_light(DriverPid, hall_down, 3, on_off(Down3)),

  elevator_interface:set_order_button_light(DriverPid, cab, 0, on_off(Cab0)),
  elevator_interface:set_order_button_light(DriverPid, cab, 1, on_off(Cab1)),
  elevator_interface:set_order_button_light(DriverPid, cab, 2, on_off(Cab2)),
  elevator_interface:set_order_button_light(DriverPid, cab, 3, on_off(Cab3)),

  elevator_interface:set_floor_indicator(DriverPid, Floor),
  timer:sleep(500),
  light_func(DriverPid).



%%%Sensing from buttons and floor sensor, high priority

sens_to_mnesia(DriverPid) ->
  get_set_cab_buttons(DriverPid),
  get_set_hall_up_buttons(DriverPid),
  get_set_hall_down_buttons(DriverPid),
  timer:sleep(25),
  sens_to_mnesia(DriverPid).

get_set_floor_sensor(DriverPid) -> 
  case elevator_interface:get_floor_sensor_state(DriverPid) of
    A when is_integer(A) ->
      storage:update_elevator_floor(node(), A),
      storage:update_elevator_is_on_floor(node(), true),
      get_set_floor_sensor(DriverPid, A);
    B ->
      storage:update_elevator_is_on_floor(node(), false),
      get_set_floor_sensor(DriverPid, B)
  end.

get_set_floor_sensor(DriverPid, FloorVar) ->
  case elevator_interface:get_floor_sensor_state(DriverPid) of
    FloorVar ->
      get_set_floor_sensor(DriverPid, FloorVar);
    A when is_integer(A) ->
      storage:update_elevator_floor(node(), A),
      storage:update_elevator_is_on_floor(node(), true),
      get_set_floor_sensor(DriverPid, A);
    B ->
      storage:update_elevator_is_on_floor(node(), false),
      get_set_floor_sensor(DriverPid, B)
  end.



get_set_cab_buttons(DriverPid) ->
  case elevator_interface:get_order_button_state(DriverPid, 0, cab) of
    1 ->
      storage:write_cab_requests(0, true);
    0 ->
      ok
  end,
  case elevator_interface:get_order_button_state(DriverPid, 1, cab) of
    1 ->
      storage:write_cab_requests(1, true);
    0 ->
      ok
  end,
  case elevator_interface:get_order_button_state(DriverPid, 2, cab) of
    1 ->
      storage:write_cab_requests(2, true);
    0 ->
      ok
  end,
  case elevator_interface:get_order_button_state(DriverPid, 3, cab) of
    1 ->
      storage:write_cab_requests(3, true);
    0 ->
      ok
  end.

get_set_hall_up_buttons(DriverPid) ->
  case elevator_interface:get_order_button_state(DriverPid, 0, hall_up) of 
    1 ->
      storage:update_hall_requests(0, up, true);
    0 -> 
      ok
  end,
  case elevator_interface:get_order_button_state(DriverPid, 1, hall_up) of 
    1 ->
      storage:update_hall_requests(1, up, true);
    0 -> 
      ok
  end,
  case elevator_interface:get_order_button_state(DriverPid, 2, hall_up) of 
    1 ->
      storage:update_hall_requests(2, up, true);
    0 -> 
      ok
  end.

get_set_hall_down_buttons(DriverPid) ->
  case elevator_interface:get_order_button_state(DriverPid, 1, hall_down) of 
    1 ->
      storage:update_hall_requests(1, down, true);
    0 -> 
      ok
  end,
  case elevator_interface:get_order_button_state(DriverPid, 2, hall_down) of 
    1 ->
      storage:update_hall_requests(2, down, true);
    0 ->
      ok
  end,
  case elevator_interface:get_order_button_state(DriverPid, 3, hall_down) of 
    1 ->
      storage:update_hall_requests(3, down, true);
    0 -> 
      ok
  end.
