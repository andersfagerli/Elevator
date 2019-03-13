-module(network).

-export([start/0]).

-define(BROADCAST_IP, {255,255,255,255}).
-define(RECV_PORT, 8971).
-define(SEND_PORT, 8972).
-define(SLEEP_PERIOD, 5000).

start() ->
  spawn(fun() -> init_broadcast() end),
  spawn(fun() -> init_receive() end),
  spawn(fun() -> debug() end).

%%% BROADCAST %%%
init_broadcast() ->
  {ok,SendSocket} = gen_udp:open(?SEND_PORT, [list, {active, false}]),
  broadcast_node(SendSocket).

broadcast_node(Socket) ->
  ok = gen_udp:send(Socket, ?BROADCAST_IP, ?RECV_PORT, atom_to_list(node())),
  timer:sleep(?SLEEP_PERIOD),
  broadcast_node(Socket).


%%% RECEIVE %%%
init_receive() ->
  {ok, RecvSocket} = gen_udp:open(?RECV_PORT, [list, {active, false}]),
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
