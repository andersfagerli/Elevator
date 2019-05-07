-module(network).

-export([start/0]).

-define(COOKIE, 'elev').
-define(BROADCAST_IP, {255,255,255,255}).
-define(RECV_PORT, 5677).
-define(SEND_PORT, 5678).
-define(SLEEP_PERIOD, 3000).

%%% Start %%%
start() ->
  erlang:set_cookie(node(),?COOKIE),
  spawn(fun() -> init_broadcast() end),
  spawn(fun() -> init_receive() end).

%%% Broadcast %%%
init_broadcast() ->
  {ok,SendSocket} = gen_udp:open(?SEND_PORT, [list, {active, false}, {broadcast, true}]),
  broadcast_node(SendSocket).

broadcast_node(Socket) ->
  ok = gen_udp:send(Socket, ?BROADCAST_IP, ?RECV_PORT, atom_to_list(node())),
  timer:sleep(?SLEEP_PERIOD),
  broadcast_node(Socket).

%%% Receive %%%
init_receive() ->
  {ok, RecvSocket} = gen_udp:open(?RECV_PORT, [list, {active, false}, {broadcast, true}]),
  connect_to_nodes(RecvSocket).

connect_to_nodes(Socket) ->
  {ok, {_Address, _Port, NodeName}} = gen_udp:recv(Socket, 0),
  Node = list_to_atom(NodeName),
  case lists:member(Node, [node()|nodes()]) of
    true ->
      ok;
    false ->
      net_adm:ping(Node),
  end,
  connect_to_nodes(Socket).
