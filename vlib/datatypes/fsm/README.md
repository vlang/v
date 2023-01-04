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
This module includes a tool for generating dot diagrams from .v source code,
that defines a FSM. The tool is located in [fsm_graph.v](tools/fsm_graph.v).

Here is an example of how to generate a .dot file with the graph and transitions:
```bash
v run vlib/datatypes/fsm/tools/fsm_graph.v -f vlib/datatypes/fsm/fsm_test.v > graph.dot
```

You can convert the generated .dot file to a PNG file with Graphviz's `dot`
conversion tool:
```bash
v run vlib/datatypes/fsm/tools/fsm_graph.v -f vlib/datatypes/fsm/fsm_test.v > graph.dot
dot -Tpng graph.dot > graph.png
xdg-open graph.png
```

You can also visualise it with Graphviz (the `dot` command) 
& ImageMagick (the `display` command):
```bash
v run vlib/datatypes/fsm/tools/fsm_graph.v -f vlib/datatypes/fsm/fsm_test.v | dot -Tpng | display
```

To view the .dot file, you can also use any of the 
[Graphviz Graphical Interfaces](https://graphviz.org/resources/#graphical-interfaces)
and `xdot` in particular:
```bash
v run vlib/datatypes/fsm/tools/fsm_graph.v -f vlib/datatypes/fsm/fsm_test.v | xdot -
```

In all of the above examples, you can replace `vlib/datatypes/fsm/fsm_test.v`
with the path to your own .v code that imports and uses `fsm`.
