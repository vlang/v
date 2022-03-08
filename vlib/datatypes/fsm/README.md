# fsm

This module implements a Finite State Machine (FSM), or State Machine in short.
The FSM is composed of states and transitions between them.
These need to be specified by the client.

## Usage

Have a look at `statemachine_test.v` for usage examples.
On each `run()`, the possible transitions from the current state are evaluated.

If the condition (specified by the client as a handler function) is true, then:
* the client-specifid `on_exit()` handler from the current state is called.
* the client-specifid `on_entry()` handler of the new state is called.
* the client-specifid `on_run()` handler of the new state is called.
 