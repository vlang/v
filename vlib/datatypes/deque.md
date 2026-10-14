# Deque

`datatypes.new_deque[T]()` creates a double-ended queue. Use `push_front` and `push_back`
to insert, and `pop_front` and `pop_back` to remove values. Both pop methods and the matching
peek methods return an error when empty. `len`, `is_empty`, and `cap` report the current size
and ring capacity; removing values keeps the allocation for reuse and releases the removed slots.

```v
import datatypes

mut deque := datatypes.new_deque[string]()
deque.push_back('last')
deque.push_front('first')
assert deque.pop_front()! == 'first'
assert deque.peek_back()! == 'last'
```

Insertion and removal take amortized O(1) time. `array()` takes O(n) time and returns a separate
array in front-to-back order. Values follow normal V copy semantics, so references stored in the
deque still refer to the same objects. Deques require external synchronization when shared
between threads.
