# Description

![Modules and interface](/Images/flowchart.png)

## Network module
* Establishes UDP connection to remote elevators for message passing
* Receives remote hall orders and elevator states
* Sends local hall orders and elevator state

## Elevator interface module
* Interface for controlling elevator hardware
* Receives motor direction, stop and start, button light signals etc.
* Sends sensor signals, button signals etc.

## Order handler module
* Controls the individual elevator based on existing orders and states of remote and local elevator
* Adds and removes orders continuously
* Stores existing orders in containers
  * One for all hall orders
  * One for local hall orders
  * One for local cab orders
* Cost function for choosing which elevator to handle new order
  * Communicates and compares cost over network to other elevators
  * Adds new order to local hall order container of chosen elevator
* Receives remote hall orders and elevator states
* Receives local elevator state
* Receives local hall and cab button pushes
* Sends local hall and cab orders

## Elevator module
* Controls the logic of the individual elevator
* Updates local state machine
* Receives local hall and cab orders
* Receives local elevator states
* Sends state information and finished orders
* Sends messages to interface for controlling elevator


## FSM module
* Holds the different elevator states and values
  * Moving
  * Idle
  * Door open
  * Direction
  * Floor
  * ...
* Receives updates in state from elevator
* Sends current state
