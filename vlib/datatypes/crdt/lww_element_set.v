module crdt

import time

// LWW-Element-Set is similar to 2P-Set in that it consists of an "add set" and a "remove set", with a timestamp for each element.
struct LWWESet[T] {
mut:
	add_map map[T]time.Time
	rm_map  map[T]time.Time
	time    time.Time
}

// new_orset returns an instance of LWWESet.
pub fn new_lwweset[T]() LWWESet[T] {
	return LWWESet[T]{
		add_map: map[T]time.Time{}
		rm_map: map[T]time.Time{}
	}
}

// add lets you add an element to set.
pub fn (mut s LWWESet[T]) add(value T) {
	s.add_map[value] = time.now()
}

// remove deletes the element from the set.
pub fn (mut s LWWESet[T]) remove(value T) {
	s.rm_map[value] = time.now()
}

// lookup returns true if an element exists within the
// set or false otherwise.
pub fn (mut s LWWESet[T]) lookup(value T) bool {
	if value in s.add_map.keys() {
		if value in s.rm_map.keys() {
			return s.rm_map[value].unix < s.add_map[value].unix
		}
		return true
	} else {
		return false
	}
	if value in s.add_map {
		return true
	}
	if value in s.rm_map {
		return true
	}
	return false
}

// merge function to merge the LWWESet object's payload with the argument's payload.
pub fn (mut s LWWESet[T]) merge(r LWWESet[T]) {
	for value, ts in r.add_map {
		if value in s.add_map {
			t := s.add_map[value]
			if t < ts {
				s.add_map[value] = ts
			}
		} else {
			s.add_map[value] = ts
		}
	}

	for value, ts in r.rm_map {
		if value in s.rm_map {
			t := s.rm_map[value]
			if t < ts {
				s.rm_map[value] = ts
			}
		} else {
			s.rm_map[value] = ts
		}
	}
}
