-module(order).

-export([
    assign_order/0
    ]).

%Format input JSON
% {
%     "hallRequests" : 
%         [[Boolean, Boolean], ...],
%     "states" : 
%         {
%             "id_1" : {
%                 "behaviour"     : < "idle" | "moving" | "doorOpen" >
%                 "floor"         : NonNegativeInteger
%                 "direction"     : < "up" | "down" | "stop" >
%                 "cabRequests"   : [Boolean, ...]
%             },
%             "id_2" : {...}
%         }
% }
%Example input JSON
% {
%     "hallRequests" : 
%         [[false,false],[true,false],[false,false],[false,true]],
%     "states" : {
%         "one" : {
%             "behaviour":"moving",
%             "floor":2,
%             "direction":"up",
%             "cabRequests":[false,false,true,true]
%         },
%         "two" : {
%             "behaviour":"idle",
%             "floor":0,
%             "direction":"stop",
%             "cabRequests":[false,false,false,false]
%         }
%     }
% }
%
%Format output JSON
% {
%     "id_1" : [[Boolean, Boolean], ...],
%     "id_2" : ...
% }
%Example output JSON
% {
%     "one" : [[false,false],[false,false],[false,false],[false,true]],
%     "two" : [[false,false],[true,false],[false,false],[false,false]]
% }
% build_json() ->
%     TempMap = #{<<"hallRequests">> =>
%                     [[false,false],[false,false],[false,false],[false,false]],
%                 <<"states">> =>
%                     #{<<"one">> =>
%                         #{<<"behaviour">> => <<"moving">>,
%                         <<"cabRequests">> => [false,false,true,true],
%                         <<"direction">> => <<"up">>,
%                         <<"floor">> => 2},
%                     <<"two">> =>
%                         #{<<"behaviour">> => <<"idle">>,
%                         <<"cabRequests">> => [false,false,false,false],
%                         <<"direction">> => <<"stop">>,
%                         <<"floor">> => 0}}},
%     TempMap.

build_elevator_map(Node) ->
    {atomic, [{elevator_states, _NodeAtom,Behavior,Direction, Floor, _IsOnFloor}]} = storage:read_elevator_states(Node),

    #{atom_to_binary(Node, utf8) =>
        #{<<"behaviour">> => atom_to_binary(Behavior, utf8),
        <<"cabRequests">> => [false,false,false,false],
        <<"direction">> => atom_to_binary(Direction, utf8),
        <<"floor">> => Floor}}.

build_state_map(Nodes) ->
    #{<<"states">> => lists:foldl(fun(A, B) -> maps:merge(A, B) end, #{}, lists:map(fun(X) -> build_elevator_map(X) end, Nodes))}.

build_hall_map() ->
    {atomic, List} = storage:select_hall_requests(),
    maps:put(<<"hallRequests">>, List, maps:new()).

%3elevator assign order
assign_order() ->
    storage:update_dist_requests(node(), maps:get(atom_to_binary(node(), utf8), jsone:decode(list_to_binary(os:cmd("./hall_request_assigner --input '" ++ binary_to_list(jsone:encode(maps:merge(build_hall_map(), build_state_map([node()|nodes()])))) ++ "'"))))),
    timer:sleep(100),
    assign_order().