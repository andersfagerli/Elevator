# Module overview

## Fsm
Holds the elevators different states:
 * Idle
 * Moving
 * Door open

and actuates elevator through events.

## Elevator
### elevator.erl
Has three tasks:
 * Floor sensing
 * Button sensing
 * Floor request handling

Forwards current floor and button pushes to storage.
Receives floor to go to from order_handler.
### elevator_interface.erl
Interfaces the hardware of the elevator

## Order handler
### order_handler.erl
Calculates the next floor the elevator should go to,
based on state of all elevators. Reads elevator states
and orders from storage and forwards next floor to elevator.
### storage.erl
Database for storing hall requests, cab requests and elevator
states. Utilizes the Erlang module **mnesia** for storing a
distributed database across all nodes, with local copies on
each node.

## Network
Connects nodes by pinging a UDP broadcast of the individual
nodes.

# Interfaces
The interfaces between the modules are done by message passing,
all going through **main.erl**'s listeners. A listener receives
messages from its associated module, and forwards it to another
module or performs an action. **main.erl** acts as a starting
point for the elevator and connection point between the modules.
