# A V implementation of CRDTs

The following state-based counters and sets have currently been implemented.

## Counters

### G-Counter

A grow-only counter (G-Counter) can only increase in one direction. The increment operation increases the value of current replica by 1. The merge operation combines values from distinct replicas by taking the maximum of each replica.

```v
gcounter := crdt.new_gcounter()

// We can increase the counter monotonically by 1.
gcounter.increment()

// Twice.
gcounter.increment()

// Or we can pass in an arbitrary delta to apply as an increment.
gcounter.increment_value(2)

// Should print '4' as the result.
println(gcounter.value())

another_counter := crdt.new_gcounter()

// We can merge counter between each other
another_counter.merge(gcounter)
gcounter.merge(another_counter)
assert another_counter.value() == gcounter.value() 
```

### PN-Counter

A positive-negative counter (PN-Counter) is a CRDT that can both increase or
decrease and converge correctly in the light of commutative
operations. Both `.increment()` and `.decrement()` operations are allowed and thus
negative values are possible as a result.

```v
pncounter := crdt.new_pncounter()

// We can increase the counter by 1.
pncounter.increment()

// Or more.
pncounter.increment_value(100)

// And similarly decrease its value by 1.
pncounter.decrement()

// Or more.
pncounter.decrement_value(100)

// End result should equal '0' here.
pncounter.value() == 0
```

## Sets

### G-Set

A grow-only (G-Set) set to which element/s can be added to. Removing element
from the set is not possible.

```v
obj := "dummy-object"
gset := crdt.new_gset<string>()

gset.add(obj)

// Should always print 'true' as `obj` exists in the g-set.
println(gset.lookup(obj))
```

### 2P-Set

Two-phase set (2P-Set) allows both additions and removals to the set.
Internally it comprises of two G-Sets, one to keep track of additions
and the other for removals.

```v
obj := "dummy-object"

twophaseset := crdt.new_two_phase_set()

twophaseset.add(obj)

// Remove the object that we just added to the set, emptying it.
twophaseset.remove(obj)

// Should return 'false' as the obj doesn't exist within the set.
twophaseset.lookup(obj) == false
```

### LWW-Element-Set

Last-write-wins element set (LWW-Element-Set) keeps track of element additions
and removals but with respect to the timestamp that is attached to each
element. Timestamps should be unique and have ordering properties.

```v
obj := "dummy-object"
lwweset := crdt.new_lwweset()

// Here, we remove the object first before we add it in. For a
// 2P-set the object would be deemed absent from the set. But for
// a LWW-set the object should be present because `.add()` follows
// `.remove()`.
lwweset.remove(obj)
lwweset.add(obj)

// This should print 'true' because of the above.
lwweset.lookup(obj) == true
```

### OR-Set

An OR-Set (Observed-Removed-Set) allows deletion and addition of
elements similar to LWW-e-Set, but does not surface only the most recent one. Additions are uniquely tracked via tags and an element is considered member of the set if the deleted set consists of all the tags present within additions.

```v
// object 1 == object 2
obj1 := "dummy-object"
obj2 := "dummy-object"

orset := crdt.new_orset()

orset.add(obj1)
orset.add(obj2)

// Removing any one of the above two objects should remove both
// because they contain the same value.
orset.remove(obj1)

// Should return 'false'.
orset.lookup(obj2) == false
```

## ToDo

- [x] Add `compare` and `merge` methods to G-Set (`gset.v`)
- [ ] Add `compare` and `merge` methods to 2P-Set (`two_phase_set.v`)
