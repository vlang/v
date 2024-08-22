module main

import (
    strconv
    io
)

enum NodeType {
    literal
    variable
    assignment
    addition
    function
    call
}

struct Node {
    typ       NodeType
    value     string
    left      &Node
    right     &Node
    children  []&Node
}

fn new_node(typ NodeType, value string, left &Node, right &Node, children []&Node) &Node {
    return &Node{
        typ: typ,
        value: value,
        left: left,
        right: right,
        children: children,
    }
}
