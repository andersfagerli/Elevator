-module(network).

-export([start/3]).

-define(BROADCAST_IP, {255,255,255,255}).
-define(LOCAL_IP, {127,0,0,1}).
-define(SLEEP_PERIOD, 5000).

%%% Ex. %%%
% elev1: RecvPort = 8971, SendPort = 8972, OtherRecvPort = 8973
% elev2: RecvPort = 8973, SendPort = 8974, OtherRecvPort = 8971

start(RecvPort, SendPort, OtherRecvPort) ->
  spawn(fun() -> init_broadcast(SendPort,OtherRecvPort) end),
  spawn(fun() -> init_receive(RecvPort) end),
  spawn(fun() -> debug() end).

%%% BROADCAST %%%
init_broadcast(SendPort,OtherRecvPort) ->
  {ok,SendSocket} = gen_udp:open(SendPort, [list, {active, false}]),
  broadcast_node(SendSocket,OtherRecvPort).

broadcast_node(Socket,OtherRecvPort) ->
  ok = gen_udp:send(Socket, ?LOCAL_IP, OtherRecvPort, atom_to_list(node())),
  timer:sleep(?SLEEP_PERIOD),
  broadcast_node(Socket,OtherRecvPort).


%%% RECEIVE %%%
init_receive(RecvPort) ->
  {ok, RecvSocket} = gen_udp:open(RecvPort, [list, {active, false}]),
  connect_to_nodes(RecvSocket).

connect_to_nodes(Socket) ->
  {ok, {Address, Port, NodeName}} = gen_udp:recv(Socket, 0),
  Node = list_to_atom(NodeName),
  net_adm:ping(Node),
  connect_to_nodes(Socket).


%%% DEBUG %%%
debug() ->
  timer:sleep(?SLEEP_PERIOD),
  case nodes() =:= [] of
    true ->
      io:format("Not connected\n");
    false ->
      io:format("Connected to: ~p~n",nodes())
  end,
  debug().
