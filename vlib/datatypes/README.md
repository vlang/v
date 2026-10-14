# datatypes

This module provides implementations of less frequently used, but still common
data types.

V's `builtin` module is imported implicitly, and has implementations for arrays,
maps and strings. These are good for many applications, but there are a plethora
of other useful data structures/containers, like linked lists, priority queues,
trees, etc, that allow for algorithms with different time complexities, which may
be more suitable for your specific application.

It is implemented using generics, that you have to specialise for the type of
your actual elements. For example:
```v
import datatypes

mut stack := datatypes.Stack[int]{}
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
- [x] Bloom filter
- [ ] ...

`heapify(items)` builds a minimum heap from a copy of the input. `pushpop(item)` returns the
smallest of the item and heap contents; an empty heap returns the item unchanged.
`replace(item)` removes the existing minimum before inserting the item and errors on an empty
heap. `nlargest(n, items)` and `nsmallest(n, items)` return sorted copies with at most `n`
elements; nonpositive counts return an empty array.
