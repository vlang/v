# datatypes.crdt

`datatypes.crdt` provides Conflict-free Replicated Data Types (CRDTs) for distributed
and concurrent applications.

CRDTs are data structures that can be replicated across multiple nodes in a network,
where replicas can be updated independently and concurrently without coordination,
and always converge to the same state when merged.

## Available Types

### Counters

#### GCounter (Grow-only Counter)
A state-based counter that only increases monotonically. Merging takes the maximum
counter value for each replica identity.

```v nofmt
import datatypes.crdt

mut c1 := crdt.new_gcounter_with_id('node_a')
mut c2 := crdt.new_gcounter_with_id('node_b')

c1.increment()
c1.increment_value(5)
c2.increment_value(3)

c1.merge(&c2)
println(c1.value()) // 9
```

#### PNCounter (Positive-Negative Counter)
A counter that supports both increments and decrements by pairing two G-Counters:
one for positive increments and one for negative decrements.

```v nofmt
import datatypes.crdt

mut pn := crdt.new_pncounter()
pn.increment_value(10)
pn.decrement_value(4)
println(pn.value()) // 6
```

### Sets

#### GSet[T] (Grow-only Set)
A set where elements can only be added, never removed. Merge is a set union.

```v nofmt
import datatypes.crdt

mut s := crdt.new_gset[string]()
s.add('apple')
println(s.lookup('apple')) // true
```

#### TwoPhaseSet[T] (2P-Set)
A two-phase set allowing additions and removals. An element once removed can never
be re-added.

```v nofmt
import datatypes.crdt

mut tps := crdt.new_two_phase_set[string]()
tps.add('item')
tps.remove('item')
println(tps.lookup('item')) // false
```

#### LWWESet[T] (Last-Write-Wins Element Set)
An element set where each addition and removal is associated with a timestamp.
Elements can be re-added and re-removed repeatedly. Timestamp ties are resolved
by configurable bias (`.add_wins` or `.remove_wins`).

```v nofmt
import datatypes.crdt

mut lww := crdt.new_lwweset[string]()
lww.add('doc')
lww.remove('doc')
lww.add('doc') // Re-added with a newer timestamp
println(lww.lookup('doc')) // true
```

#### ORSet[T] (Observed-Removed Set / Add-Wins Set)
An observed-removed set that uniquely tracks additions with internal tags.
Removing an element only removes the tags observed so far at that replica.
Concurrent additions and removals result in additions winning (Add-Wins).

```v nofmt
import datatypes.crdt

mut orset := crdt.new_orset[string]()
orset.add('doc')
orset.remove('doc')
orset.add('doc') // Re-added with a fresh unique tag
println(orset.lookup('doc')) // true
```

