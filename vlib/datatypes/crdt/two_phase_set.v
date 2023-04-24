module crdt

// TwoPhaseSet supports both addition and removal of
// elements to set.
struct TwoPhaseSet[T] {
mut:
	add_set GSet[T]
	rm_set  GSet[T]
}

// new_two_phase_set returns a new instance of TwoPhaseSet.
fn new_two_phase_set[T]() TwoPhaseSet[T] {
	return TwoPhaseSet[T]{
		add_set: new_gset[T]()
		rm_set: new_gset[T]()
	}
}

// add inserts element into the TwoPhaseSet.
fn (mut t TwoPhaseSet[T]) add(elem T) {
	t.add_set.add(elem)
}

// remove deletes the element from the set.
fn (mut t TwoPhaseSet[T]) remove(elem T) {
	t.rm_set.add(elem)
}

// lookup returns true if the set contains the element.
// The set is said to contain the element if it is present
// in the add-set and not in the remove-set.
fn (mut t TwoPhaseSet[T]) lookup(elem T) bool {
	return t.add_set.lookup(elem) && !t.rm_set.lookup(elem)
}
