# fsm

This module implements a Finite State Machine (FSM).
The FSM is composed of states and transitions between them.
These need to be specified by the client.

## Usage

Have a look at `fsm_test.v` for usage examples.

On each `run()`, all the possible transitions from the current state are evaluated.
The first transition for the current state, whose condition evaluates to true is
taken (the condition is specified by a transition callback function). 

In a successfull transition, the current state changes to the new one.
When that happens:
* the client-specified `on_exit()` handler from the current state is called.
* the client-specified `on_entry()` handler of the new state is called.

After all transitions are checked, and thus the state is changed, the client-specified
`on_run()` handler of the now current state is called.

## Plot States and Transitions

First install dependencies:
* Linux `sudo apt install xdot`.

Example of how to generate the graph and transitions:
```bash
xdot <(v run fsm_graph.v -f fsm_test.v)
```
