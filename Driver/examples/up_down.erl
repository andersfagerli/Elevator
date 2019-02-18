-module(up_down).

-export([start/0]).

start() ->
    {ok, DriverPid} = elevator_interface:start(),
    go_up(DriverPid).

go_up(DriverPid) ->
    elevator_interface:set_motor_direction(DriverPid, up),
    timer:sleep(20),
    case elevator_interface:get_floor_sensor_state(DriverPid) of
	3 ->
	    go_down(DriverPid);
	_ ->
	    go_up(DriverPid)
    end.
	    
go_down(DriverPid) ->
    elevator_interface:set_motor_direction(DriverPid, down),
    timer:sleep(20),
    case elevator_interface:get_floor_sensor_state(DriverPid) of
	0 ->
	    go_up(DriverPid);
	_ ->
	    go_down(DriverPid)
    end.
	    
