module crdt

// PNCounter represents a state-based Positive-Negative Counter (PN-Counter) in CRDT.
// It consists of two G-Counters: one tracking increments and the other tracking decrements.
// It can both increase and decrease and converges across replicas.
pub struct PNCounter {
mut:
	p_counter GCounter
	n_counter GCounter
}

// new_pncounter creates a new PNCounter with randomly generated identities.
pub fn new_pncounter() PNCounter {
	return PNCounter{
		p_counter: new_gcounter()
		n_counter: new_gcounter()
	}
}

// new_pncounter_with_id creates a new PNCounter with the specified replica identity.
pub fn new_pncounter_with_id(id string) PNCounter {
	return PNCounter{
		p_counter: new_gcounter_with_id(id)
		n_counter: new_gcounter_with_id(id)
	}
}

// id returns the replica identity of this PN-Counter.
pub fn (pn &PNCounter) id() string {
	return pn.p_counter.id()
}

// increment increments the counter value by 1.
pub fn (mut pn PNCounter) increment() {
	pn.increment_value(1)
}

// increment_value increments the counter value by the provided positive delta.
// The value must be non-negative.
pub fn (mut pn PNCounter) increment_value(incr int) {
	pn.p_counter.increment_value(incr)
}

// decrement decrements the counter value by 1.
pub fn (mut pn PNCounter) decrement() {
	pn.decrement_value(1)
}

// decrement_value decrements the counter value by the provided positive delta.
// The value must be non-negative.
pub fn (mut pn PNCounter) decrement_value(decr int) {
	pn.n_counter.increment_value(decr)
}

// value returns the current effective value of the PN-Counter (positive minus negative).
pub fn (pn &PNCounter) value() int {
	return pn.p_counter.value() - pn.n_counter.value()
}

// merge combines both the positive and negative G-Counters of two PNCounters.
pub fn (mut pn PNCounter) merge(other &PNCounter) {
	pn.p_counter.merge(&other.p_counter)
	pn.n_counter.merge(&other.n_counter)
}

