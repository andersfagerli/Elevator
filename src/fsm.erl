-module(fsm).
-export([start/1,
        send_new_floor/2,
        send_order_finished/1,
        send_door_open/1,
        send_order_direction/2]).

%%% Start %%%
start(FsmListener) ->
  spawn(fun() -> init(FsmListener) end).

%%% Init %%%
init(FsmListener) ->
  event_init(FsmListener),
  receive
    {new_floor, Floor} ->
      idle(FsmListener)
  end.

%%% States %%%
idle(FsmListener) ->
  event_stop(FsmListener),
  receive
    {door, open} ->
      door_open(FsmListener);
    {order, Direction} ->
      moving(FsmListener, Direction)
  end.

moving(FsmListener, Direction) ->
  event_moving(FsmListener,Direction),
  receive
    {order, finished} ->
      idle(FsmListener);
    {order, NewDirection} ->
      moving(FsmListener, NewDirection)
  end.

door_open(FsmListener) ->
  event_open_door(FsmListener),
  timer:sleep(3000),
  event_close_door(FsmListener),
  idle(FsmListener).



%%% Events %%%
event_init(FsmListener)                 -> FsmListener ! init.
event_stop(FsmListener)                 -> FsmListener ! {drive,stop}.
event_open_door(FsmListener)            -> FsmListener ! {door,open}.
event_close_door(FsmListener)           -> FsmListener ! {door,close}.
event_moving(FsmListener, Direction)    -> FsmListener ! {drive,Direction}.

%%% Module interface %%%
send_door_open(FsmPid)                  -> FsmPid ! {door, open}.
send_order_finished(FsmPid)             -> FsmPid ! {order, finished}.
send_new_floor(FsmPid, Floor)           -> FsmPid ! {new_floor, Floor}.
send_order_direction(FsmPid, Direction) -> FsmPid ! {order, Direction}.
