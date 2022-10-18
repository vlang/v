# datatypes

This module provides implementations of less frequently used, but still common
data types.

V's `builtin` module is imported implicitly, and has implementations for arrays,
maps and strings. These are good for many applications, but there are a plethora
of other useful data structures/containers, like linked lists, priority queues,
tries, etc, that allow for algorithms with different time complexities, which may
be more suitable for your specific application.

It is implemented using generics, that you have to specialise for the type of
your actual elements. For example:
```v
import datatypes

mut stack := datatypes.Stack<int>{}
stack.push(1)
println(stack)
```

## Currently Implemented Datatypes:

- [x] Linked list
- [x] Doubly linked list
- [x] Stack (LIFO)
- [x] Queue (FIFO)
- [x] Min heap (priority queue)
- [x] Set
- [x] Quadtree
- [ ] ...
