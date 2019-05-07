-module(fsm).
-export([
  move/2,
  stop/1,
  close_door/1,
  idle/1,
  open_door/1]).

%%% States %%%
idle(FsmListener) ->
  FsmListener ! {direction, stop},
  storage:update_elevator_behavior(node(), idle),
  storage:update_elevator_direction(node(), stop),
  %%order:assign_order(),
  receive
    {door, open} ->
      door_open(FsmListener);
    {direction, Direction} ->
      moving(FsmListener, Direction);
    _ ->
      idle(FsmListener)
  end.

moving(FsmListener, Direction) ->
  FsmListener ! {direction, Direction},
  storage:update_elevator_behavior(node(), moving),
  storage:update_elevator_direction(node(), Direction),
  %%order:assign_order(),
  receive
    {direction, stop} ->
      idle(FsmListener);
    {direction, NewDirection} ->
      moving(FsmListener, NewDirection);
    {door, open} ->
      door_open(FsmListener);
    _ ->
      moving(FsmListener, Direction)
  after
    15000 ->
      io:fwrite("MOTOR ERROR, Halting program"),
      halt()
  end.

door_open(FsmListener) ->
  FsmListener ! {direction, stop},
  FsmListener ! {door,open},
  storage:update_elevator_behavior(node(), doorOpen),
  %%order:assign_order(),
  receive
    {door,close} ->
      FsmListener ! {door,close},
      idle(FsmListener);
    {direction, stop} ->
      FsmListener ! {door,close},
      idle(FsmListener);
    {direction, Direction}->
      FsmListener ! {door,close},
      moving(FsmListener, Direction);
    _ ->
      door_open(FsmListener)
  end.

% Messages
% {direction, up}
% {direction, down}
% {direction, stop}
% {door, open}
% {door,open}
%%% Events %%%
move(Fsm, Direction) -> Fsm ! {direction, Direction}.
stop(Fsm) -> Fsm ! {direction, stop}.
close_door(Fsm) -> Fsm ! {door,close}.
open_door(Fsm) -> Fsm ! {door,open}.
