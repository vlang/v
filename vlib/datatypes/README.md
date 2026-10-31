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
- [x] BTree ordered map and BTreeSet ordered set
- [x] Quadtree
- [x] Bloom filter
- [x] CRDT Conflict-free replicated data types (`datatypes.crdt`)
- [ ] ...

`heapify(items)` builds a minimum heap from a copy of the input. `pushpop(item)` returns the
smallest of the item and heap contents; an empty heap returns the item unchanged.
`replace(item)` removes the existing minimum before inserting the item and errors on an empty
heap. `nlargest(n, items)` and `nsmallest(n, items)` return sorted copies with at most `n`
elements; nonpositive counts return an empty array.

## Directed graphs

`new_graph[T]()` creates a directed graph with V map keys as nodes.
`add_node` adds an isolated node; `add_edge(from, to)` adds both endpoints implicitly.
Parallel edges are accepted. `successors(node)` returns an independent array.
`topo_sort()` returns a result containing an ordering where each edge's source precedes its target.
`has_cycle()` detects cycles, including self-loops. An empty graph has an empty ordering.
Cycle errors list nodes that could not be ordered, including nodes downstream of a cycle.

## Ordered B-trees

`new_btree[K, V]()` creates an ordered map with logarithmic insert, lookup, and delete.
Keys must have consistent equality and a total order; floating-point NaN keys are unsupported.
`set(key, value)` inserts or replaces an entry, `get(key)` returns an optional value,
and `delete(key)` reports whether it removed an entry. `len()` and `is_empty()` describe its size.
`min_key()` and `max_key()` return optional bounds; `keys()` returns all keys in ascending order.

`range_keys(lo, hi)` and `iterate(lo, hi)` include both bounds.
`iterate` returns `BTreeIter[BTreeEntry[K, V]]`, with `next()` returning an optional entry.
The iterator captures entries when it is created; subsequent insertion or removal does not
change that entry list. Values use ordinary V assignment semantics, without deep-cloning
references inside them. `next()` returns `none` when the captured entries are exhausted.

`new_btree_set[T]()` creates an ordered set with `add`, `contains`, `remove`, `len`,
`is_empty`, `min`, `max`, and inclusive `range(lo, hi)` methods.
`add` and `remove` report whether membership changed.
