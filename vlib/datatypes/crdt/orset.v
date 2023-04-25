module crdt

import rand

// ORSet resembles LWWESet, but using unique tags instead of timestamps.
struct ORSet[T] {
mut:
	add_map map[T]map[string]T
	rm_map  map[T]map[string]T
}

// new_orset returns an instance of ORSet.
pub fn new_orset[T]() ORSet[T] {
	return ORSet[T]{
		add_map: map[T]map[string]T{}
		rm_map: map[T]map[string]T{}
	}
}

// add lets you add an element to set.
pub fn (mut o ORSet[T]) add(value T) {
	if value in o.add_map {
		o.add_map[value][rand.ulid().str()] = value
		return
	}
	o.add_map[value][rand.ulid().str()] = value
}

// remove deletes the element from the set.
pub fn (mut o ORSet[T]) remove(value T) {
	if value in o.add_map {
		for uid, _ in o.add_map[value] {
			o.rm_map[value][uid] = value
		}
	}
	o.rm_map[value][rand.ulid().str()] = value
}

// lookup returns true if an element exists within the
// set or false otherwise.
pub fn (mut o ORSet[T]) lookup(value T) bool {
	if value in o.add_map {
		if value in o.rm_map {
			for uid, _ in o.add_map {
				if uid !in o.rm_map {
					return true
				}
			}
		} else {
			return true
		}
	} else {
		return false
	}
	return false
}

// merge function to merge the ORSet object's payload with the argument's payload.
pub fn (mut o ORSet[T]) merge(r ORSet[T]) {
	for value, m in r.add_map {
		if add_map := o.add_map[value] {
			for uid, _ in m {
				add_map[uid]
			}
		}
		o.add_map[value] = m.clone()
	}
	for value, m in r.rm_map {
		if rm_map := o.rm_map[value] {
			for uid, _ in m {
				rm_map[uid]
			}
		}
		o.rm_map[value] = m.clone()
	}
}
