-module(main).


-export([
start/0,
start/2,
start/3,
fsm_listener/1,
elev_algo/1,
process_trapper/3,
restart_process/3
]).
%-export([send_order/1]). %For debugging purposes

%default start func
start() ->
  start(1, 1, 15657).

start(Cold, ElevNumber) ->
  start(Cold, ElevNumber, 15657).

%%start processes and register pid names
start(Cold, ElevNumber, Port) ->

  %network:start(),
  %%spawn(network, init_broadcast, []),
  spawn(main, process_trapper, [network, init_broadcast, []]),
  %%spawn(network, init_receive, []),
  spawn(main, process_trapper, [network, init_receive, []]),
  %timer:sleep(1000),
  wait_till_connected(ElevNumber),
  storage:init(ElevNumber),
  timer:sleep(1000),
  storage:init_tables(Cold),
  timer:sleep(1000),
  %15657
  {ok, DriverPid} = elevator_interface:start({127,0,0,1}, Port), %start elevator_interface
  register(driver, DriverPid),

  %code for waiting till on a floor
  elevator_interface:set_motor_direction(DriverPid, down),
  wait_till_floor(DriverPid),
  elevator_interface:set_motor_direction(DriverPid, stop),

  % start process for sensing buttons and floor sensosr
  %%spawn(elev, sens_to_mnesia, [DriverPid]),
  spawn(main, process_trapper, [elev, sens_to_mnesia, [DriverPid]]),
  %%spawn(elev, get_set_floor_sensor, [DriverPid]),
  spawn(main, process_trapper, [elev, get_set_floor_sensor, [DriverPid]]),
  %%spawn(elev, light_func, [DriverPid]),
  spawn(main, process_trapper, [elev, light_func, [DriverPid]]),

   %process for fsm_listener
  FsmListener = spawn(main, fsm_listener, [DriverPid]),
  FsmPid = spawn(fsm, idle, [FsmListener]), %process for fsm
  register(fsm, FsmPid),
  spawn(main, process_trapper, [order, assign_order, []]),


  %%spawn(main, elev_algo, [FsmPid]),
  spawn(main, process_trapper, [main, elev_algo, [FsmPid]]).
  %%elev:start_elevator_logic(FsmPid).


wait_till_floor(DriverPid) ->
  timer:sleep(50),
  case elevator_interface:get_floor_sensor_state(DriverPid) of
    A when is_integer(A) ->
      ok;
    _ ->
      wait_till_floor(DriverPid)
      
  end.


process_trapper(ModuleAtom, ProcessAtom, ArgList) -> 
  process_flag(trap_exit, true),
  spawn_link(ModuleAtom, ProcessAtom,ArgList),
  restart_process(ModuleAtom, ProcessAtom, ArgList).

restart_process(ModuleAtom, ProcessAtom, ArgList) ->
  receive
    {'EXIT', _Pid, _Reason} ->
      spawn_link(ModuleAtom, ProcessAtom, ArgList),
      restart_process(ModuleAtom, ProcessAtom, ArgList)
  end.


wait_till_connected(ElevNumber) ->
  case length([node()|nodes()]) of
    ElevNumber ->
      ok;
    _ ->
      wait_till_connected(ElevNumber)
  end.


fsm_listener(DriverPid) ->
  receive
    {direction, stop} ->
      elevator_interface:set_motor_direction(DriverPid, stop);
    {direction, up} ->
      elevator_interface:set_motor_direction(DriverPid, up);
    {direction, down} ->
      elevator_interface:set_motor_direction(DriverPid, down);
    {door, open} ->
      elevator_interface:set_door_open_light(DriverPid, on);
    {door, close} ->
      elevator_interface:set_door_open_light(DriverPid, off)
  end,
fsm_listener(DriverPid).

elev_algo(Fsm) ->
  {atomic, [{elevator_states, _,_Behavior, Direction, Floor, IsOnFloor}]} = storage:read_elevator_states(node()),
  {atomic, [{dist_requests, _, DistList}]} = storage:read_dist_requests(node()),
  {atomic, CabList} = storage:select_cab_requests(),
  case IsOnFloor of
    true ->
      stop_handler(Fsm, DistList, CabList, Direction, Floor),
      choose_direction(Fsm,DistList,CabList, Direction, Floor),
      timer:sleep(50);
    false ->
      ok
  end,
  % choose_direction(Fsm,DistList,CabList, Direction, Floor, IsOnFloor),
  % timer:sleep(50),
  % stop_handler(Fsm, DistList, CabList, Direction, Floor, IsOnFloor),
  % timer:sleep(50),
  elev_algo(Fsm).
  

stop_handler(Fsm, DistList, CabList, Direction, Floor) ->
  CurrentCab = lists:nth(Floor+1, CabList),
  CurrentDistList = lists:nth(Floor+1, DistList),
      case Direction of
        up ->
          [CurrentDist, _] = CurrentDistList,
          case CurrentDist or CurrentCab of 
            true ->
              fsm:open_door(Fsm),
              storage:clear_requests(Floor, up),
              timer:sleep(1000);
            false ->
              ok
          end;
        down ->
          [_, CurrentDist] = CurrentDistList,
          case CurrentDist or CurrentCab of 
            true ->
              fsm:open_door(Fsm),
              storage:clear_requests(Floor, down),
              timer:sleep(1000);
            false ->
              ok
          end;
        stop ->
          case lists:member(true, [CurrentCab | CurrentDistList]) of
            true -> 
              fsm:open_door(Fsm),
              storage:clear_all_floor_requests(Floor),
              timer:sleep(1000);
            false ->
              ok
          end
      end.

choose_direction(Fsm,DistList,CabList, Direction, Floor) ->
  {DistUnderList, DistTempList} = lists:split(Floor, DistList), 
  {_, DistOverList} = lists:split(1, DistTempList),
  {CabUnderList, CabTempList} = lists:split(Floor, CabList), 
  {_, CabOverList} = lists:split(1, CabTempList),
  OrderUpList = lists:flatten(DistOverList, CabOverList),
  OrderDownList = lists:flatten(DistUnderList, CabUnderList),
  OrderUp = lists:member(true, OrderUpList),
  OrderDown = lists:member(true, OrderDownList),
        case Direction of
          up ->
            case {OrderUp, OrderDown} of
              {true, true} ->
                fsm:move(Fsm, up);
              {true, false} ->
                fsm:move(Fsm, up);
              {false, true} ->
                fsm:move(Fsm, down);
              {false, false} ->
                fsm:stop(Fsm)
            end;
          down ->
            case {OrderDown, OrderUp} of
              {true, true} ->
                fsm:move(Fsm, down);
              {true, false} ->
                fsm:move(Fsm, down);
              {false, true} ->
                fsm:move(Fsm,up);
              {false, false} ->
                fsm:stop(Fsm)
            end;
          stop ->
            case {OrderUp, OrderDown} of
              {true, true} ->
                fsm:move(Fsm, up);
              {true, false} ->
                fsm:move(Fsm, up);
              {false, true} ->
                fsm:move(Fsm, down);
              {false, false} ->
                fsm:stop(Fsm)
            end
        end.
  