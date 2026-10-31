module crdt

import rand

// GCounter represents a state-based grow-only counter (G-Counter) in CRDT.
// It only supports monotonic increments and converges across replicas by taking
// the maximum value observed for each replica identity.
pub struct GCounter {
	// identity provides a unique identity for this replica.
	identity string
mut:
	// counter maps the identity of each replica to its observed counter value.
	counter map[string]int
}

// new_gcounter creates a new GCounter with a randomly generated unique identity.
pub fn new_gcounter() GCounter {
	return GCounter{
		identity: rand.ulid().str()
		counter:  map[string]int{}
	}
}

// new_gcounter_with_id creates a new GCounter with a specified replica identity.
pub fn new_gcounter_with_id(id string) GCounter {
	return GCounter{
		identity: id
		counter:  map[string]int{}
	}
}

// id returns the replica identity of this counter.
pub fn (g &GCounter) id() string {
	return g.identity
}

// increment increments the counter of this replica by 1.
pub fn (mut g GCounter) increment() {
	g.increment_value(1)
}

// increment_value increments the counter of this replica by the given value.
// The value must be non-negative.
pub fn (mut g GCounter) increment_value(val int) {
	if val < 0 {
		panic('cannot decrement a GCounter')
	}
	g.counter[g.identity] += val
}

// value returns the total accumulated value across all known replicas.
pub fn (g &GCounter) value() int {
	mut total := 0
	for _, count in g.counter {
		total += count
	}
	return total
}

// merge combines the state of another GCounter into this counter.
// For every replica, the maximum count between both counters is retained.
// This operation is commutative, associative, and idempotent.
pub fn (mut g GCounter) merge(other &GCounter) {
	for key, val in other.counter {
		if key !in g.counter || g.counter[key] < val {
			g.counter[key] = val
		}
	}
}
