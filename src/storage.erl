-module(storage).
-include_lib ("mnesia/src/mnesia.hrl").

-export([init/1]).

-export([
        update_hall_requests/3,
        update_elevator_behavior/2,
        update_elevator_direction/2,
        update_elevator_floor/2,
        update_elevator_is_on_floor/2,
        write_cab_requests/2,
        update_dist_requests/2,
        clear_requests/2,
        clear_all_floor_requests/1,
        init_tables/1
        ]).

-export([
        read_hall_requests/1,
        read_elevator_states/1,
        read_cab_requests/1,
        read_dist_requests/1,
        select_hall_requests/0,
        select_cab_requests/0
        ]).

-record(hall_requests, {
                        floor = 0, 
                        directionup = false,
                        directiondown = false}). %[up, down]

-record(elevator_states, {
                        name,
                        behavior = idle, % idle, moving, doorOpen
                        direction = stop,      % up, down, stop
                        floor = 0,
                        is_on_floor = false}).

-record(cab_requests, {
                        floor = 0,
                        stop = false}).

-record(dist_requests, {name,
                        list = [[false,false],[false,false],[false,false],[false,false]]}). %[up, down]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%REMEMBER TO CHANGE TO 3 FOR FAT%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%-define(NUMBER_OF_ELEVATORS, 1).
%%% Initialize database %%%
%%Number is number of nodes
init(Number) ->
    Nodes = [node()|nodes()],
    case length(Nodes) == Number of
        true ->
            mnesia:create_schema(Nodes),
            rpc:multicall(Nodes, mnesia, start, []),
            

            mnesia:create_table(hall_requests, [
                                        {record_name, hall_requests},
                                        {attributes, record_info(fields, hall_requests)},
                                        {disc_copies, Nodes},
                                        {type, ordered_set}
                                        ]),
            mnesia:create_table(elevator_states, [
                                        {record_name, elevator_states},
                                        {attributes, record_info(fields, elevator_states)},
                                        {disc_copies, Nodes}
                                        ]),
            mnesia:create_table(cab_requests, [
                                        {record_name, cab_requests},
                                        {attributes, record_info(fields, cab_requests)},
                                        {disc_copies, Nodes},
                                        {local_content, true},
                                        {type, ordered_set}
                                        ]),
            mnesia:create_table(dist_requests, [
                                        {record_name, dist_requests},
                                        {attributes, record_info(fields, dist_requests)},
                                        {disc_copies, Nodes},
                                        %%{local_content, true},
                                        {type, ordered_set}
                                        ]),
            %%wait for tables, might break, not sure if in right place, wait 5 seconds.
            mnesia:wait_for_tables([hall_requests, elevator_states, cab_requests, dist_requests], 5000);
            %%init table here after create
            %%Cold reset
        %rpc:multicall(Nodes, storage, init_tables, [1]);
            
            
        false ->
            io:format("Waiting for other elevator to initialise")
    end.

init_tables(Cold) ->
    case Cold of 
                1 ->
                    init_hall_requests(0),
                    init_hall_requests(1),
                    init_hall_requests(2),
                    init_hall_requests(3),
            
                    init_elevator_state(node()),

                    write_cab_requests(0, false),
                    write_cab_requests(1, false),
                    write_cab_requests(2, false),
                    write_cab_requests(3, false),

                    init_dist_requests(node());
                _ -> 
                    ok
            end.

%%% Module interface %%%
%%integer, atom, bool
init_hall_requests(Floor) ->
    F = fun() ->
        mnesia:write(#hall_requests{floor = Floor})
    end,
    mnesia:transaction(F).


update_hall_requests(Floor, Direction, Boolean) ->
    F = fun() ->    
        case Direction of
            up ->
                [Temp] = mnesia:wread({hall_requests, Floor}),
                mnesia:write(Temp#hall_requests{directionup = Boolean});
            down ->
                [Temp] = mnesia:wread({hall_requests, Floor}),
                mnesia:write(Temp#hall_requests{directiondown = Boolean})
        end
    end,
    mnesia:transaction(F).


init_elevator_state(Name) ->
    F = fun() ->
        mnesia:write(#elevator_states{name = Name})
    end,
    mnesia:transaction(F).
%% binary, binary
update_elevator_behavior(Name, Behavior) ->
    F = fun() ->    
        [Temp] = mnesia:wread({elevator_states, Name}),
        mnesia:write(Temp#elevator_states{behavior = Behavior})
    end,
    mnesia:transaction(F).

%% binary, binary
update_elevator_direction(Name, Direction) ->
    F = fun() ->    
        [Temp] = mnesia:wread({elevator_states, Name}),
        mnesia:write(Temp#elevator_states{direction = Direction})
    end,
    mnesia:transaction(F).

%%binary, integer
update_elevator_floor(Name, Floor) ->
    F = fun() ->    
        [Temp] = mnesia:wread({elevator_states, Name}),
        mnesia:write(Temp#elevator_states{floor = Floor})
    end,
    mnesia:transaction(F).

update_elevator_is_on_floor(Name, IsOnFloor) ->
    F = fun() ->    
        [Temp] = mnesia:wread({elevator_states, Name}),
        mnesia:write(Temp#elevator_states{is_on_floor = IsOnFloor})
    end,
    mnesia:transaction(F).

%%integer, bool
write_cab_requests(Floor, Stop) ->
    F = fun() -> 
        mnesia:write(#cab_requests{floor = Floor, stop = Stop})
    end,
    mnesia:transaction(F).


init_dist_requests(Name) ->
    F = fun() ->
        mnesia:write(#dist_requests{name = Name})
    end,
    mnesia:transaction(F).
%%integer, atom, bool

update_dist_requests(Name , List) ->
    F = fun() ->
        mnesia:write(#dist_requests{name = Name, list = List})
    end,
    mnesia:transaction(F).

% update_dist_requests(Floor, Direction, Boolean) ->
%     F = fun() ->    
%         case Direction of
%             up ->
%                 [Temp] = mnesia:wread({dist_requests, Floor}),
%                 mnesia:write(Temp#dist_requests{directionup = Boolean});
%             down ->
%                 [Temp] = mnesia:wread({dist_requests, Floor}),
%                 mnesia:write(Temp#dist_requests{directiondown = Boolean})
%         end
%     end,
%     mnesia:transaction(F).

clear_requests(Floor, Direction) ->
    update_hall_requests(Floor, Direction, false),
    write_cab_requests(Floor, false).

clear_all_floor_requests(Floor) ->
    update_hall_requests(Floor, up, false),
    update_hall_requests(Floor, down, false),
    write_cab_requests(Floor, false).

read_hall_requests(Floor) ->
    F = fun() -> 
    mnesia:read(hall_requests, Floor)
    end,
    mnesia:transaction(F).


%%{atomic,[{elevator_states,_, _, _, Floor, _}]} = storage:read_elevator_states(node())
read_elevator_states(Name) ->
    F = fun() -> 
    mnesia:read(elevator_states, Name)
    end,
    mnesia:transaction(F).

read_cab_requests(Floor) ->
    F = fun() -> 
    mnesia:read(cab_requests, Floor)
    end,
    mnesia:transaction(F).

read_dist_requests(Name) ->
    F = fun() -> 
    mnesia:read(dist_requests, Name)
    end,
    mnesia:transaction(F).

select_hall_requests() ->
    MatchHead = #hall_requests{floor='_', directionup='$1', directiondown='$2'},
    _Guard = {},
    Result = ['$1', '$2'],
    F = fun() -> 
    mnesia:select(hall_requests,[{MatchHead, [], [Result]}])
    end,
    mnesia:transaction(F).

select_cab_requests() ->
    MatchHead = #cab_requests{floor='_', stop='$1'},
    _Guard = {},
    Result = '$1',
    F = fun() -> 
    mnesia:select(cab_requests,[{MatchHead, [], [Result]}])
    end,
    mnesia:transaction(F).

% select_dist_requests() ->
%     MatchHead = #dist_requests{floor='_', directionup='$1', directiondown='$2'},
%     _Guard = {},
%     Result = ['$1', '$2'],
%     F = fun() -> 
%     mnesia:select(dist_requests,[{MatchHead, [], [Result]}])
%     end,
%     mnesia:transaction(F).