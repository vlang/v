module crdt

// TwoPhaseSet represents a Two-Phase Set (2P-Set) in CRDT.
// It consists of two Grow-only Sets (GSets): one for additions and one for removals.
// An element can be added, and subsequently removed. Once removed, an element cannot be re-added.
pub struct TwoPhaseSet[T] {
mut:
	add_set GSet[T]
	rm_set  GSet[T]
}

// new_two_phase_set creates a new empty TwoPhaseSet.
pub fn new_two_phase_set[T]() TwoPhaseSet[T] {
	return TwoPhaseSet[T]{
		add_set: new_gset[T]()
		rm_set:  new_gset[T]()
	}
}

// add inserts an element into the add-set.
pub fn (mut t TwoPhaseSet[T]) add(elem T) {
	t.add_set.add(elem)
}

// remove marks an element as removed by adding it to the remove-set.
pub fn (mut t TwoPhaseSet[T]) remove(elem T) {
	t.rm_set.add(elem)
}

// lookup returns true if the element is in the add-set and not in the remove-set.
pub fn (t &TwoPhaseSet[T]) lookup(elem T) bool {
	return t.add_set.lookup(elem) && !t.rm_set.lookup(elem)
}

// elements returns an array of all active elements currently belonging to the set.
pub fn (t &TwoPhaseSet[T]) elements() []T {
	mut res := []T{}
	for elem in t.add_set.elements() {
		if !t.rm_set.lookup(elem) {
			res << elem
		}
	}
	return res
}

// len returns the count of active elements currently belonging to the set.
pub fn (t &TwoPhaseSet[T]) len() int {
	return t.elements().len
}

// == checks whether two TwoPhaseSets have identical add-sets and remove-sets.
pub fn (l TwoPhaseSet[T]) == (r TwoPhaseSet[T]) bool {
	return l.add_set == r.add_set && l.rm_set == r.rm_set
}

// merge combines both the add-set and remove-set with another TwoPhaseSet.
pub fn (mut t TwoPhaseSet[T]) merge(other &TwoPhaseSet[T]) {
	t.add_set.merge(&other.add_set)
	t.rm_set.merge(&other.rm_set)
}

