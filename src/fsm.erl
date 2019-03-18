-module(fsm).
-export([start/0]).


start(Pid) ->
  spawn(fun() -> init(Pid) end).

%%% Init %%%
init(Pid) ->
  Pid ! init,
  receive
    {at_floor, Floor} ->
      Pid ! init_complete,
      idle(Pid)
  end.

%%% States %%%
idle(Pid) ->
  receive
    open_door ->
      door_open(Pid);
    handle_order ->
      moving(Pid)
  end.

moving(Pid) ->
  receive
    finished_order ->
      idle(Pid)
  end.

door_open(Pid) ->
  receive
    close_door ->
      idle(Pid)
  end.

%%% Events %%%
open_door(Pid) -> Pid ! open_door.
close_door(Pid) -> Pid ! close_door.
handle_order(Pid) -> Pid ! handle_order.
