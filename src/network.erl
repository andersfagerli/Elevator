-module(network).

-export([init_broadcast/1, init_broadcast/0,
init_receive/1, init_receive/0]).

-define(BROADCAST_IP, {255,255,255,255}).
-define(RECV_PORT, 8971).
-define(SEND_PORT, 8972).
-define(SLEEP_PERIOD, 500).


%%% BROADCAST %%%
init_broadcast() ->
  {ok,SendSocket} = gen_udp:open(?SEND_PORT, [list, {active, false}, {broadcast, true}]),
  broadcast_node(SendSocket).

init_broadcast(SendPort) ->
  {ok,SendSocket} = gen_udp:open(SendPort, [list, {active, false}, {broadcast, true}]),
  broadcast_node(SendSocket).

broadcast_node(Socket) ->
  ok = gen_udp:send(Socket, ?BROADCAST_IP, ?RECV_PORT, atom_to_list(node())),
  timer:sleep(?SLEEP_PERIOD),
  broadcast_node(Socket).


%%% RECEIVE %%%
init_receive() ->
  {ok, RecvSocket} = gen_udp:open(?RECV_PORT, [list, {active, false}]),
  connect_to_nodes(RecvSocket).

init_receive(RecvPort) ->
  {ok, RecvSocket} = gen_udp:open(RecvPort, [list, {active, false}]),
  connect_to_nodes(RecvSocket).

connect_to_nodes(Socket) ->
  {ok, {_Address, _Port, NodeName}} = gen_udp:recv(Socket, 0),
  Node = list_to_atom(NodeName),
  case lists:member(Node, [node()|nodes()]) of
    true ->
      ok;
    false ->
      net_adm:ping(Node),
      case mnesia:system_info(is_running) of
        yes ->
          %%MIGHT BE BUGGY DELETE IF PROBLEMS!!!!
          %%mnesia:stop(),
          %%timer:sleep(100),
          %%mnesia:start();
          ok;
        _ ->
          ok
      end
  end,
  timer:sleep(500),


  connect_to_nodes(Socket).
