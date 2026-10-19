module crdt

import time

// LWWBias specifies the conflict resolution tie-breaker when add and remove timestamps are equal.
pub enum LWWBias {
	add_wins
	remove_wins
}

// LWWESet represents a Last-Write-Wins Element Set (LWW-Element-Set) in CRDT.
// Each addition and removal associates a timestamp (nanoseconds) with the element.
// In contrasts to 2P-Set, elements can be repeatedly added and removed.
pub struct LWWESet[T] {
pub:
	bias LWWBias = .add_wins
mut:
	add_map map[T]i64
	rm_map  map[T]i64
}

// new_lwweset creates a new empty LWWESet with the default add-wins bias.
pub fn new_lwweset[T]() LWWESet[T] {
	return LWWESet[T]{
		bias:    .add_wins
		add_map: map[T]i64{}
		rm_map:  map[T]i64{}
	}
}

// new_lwweset_with_bias creates a new empty LWWESet with the specified bias.
pub fn new_lwweset_with_bias[T](bias LWWBias) LWWESet[T] {
	return LWWESet[T]{
		bias:    bias
		add_map: map[T]i64{}
		rm_map:  map[T]i64{}
	}
}

// add adds an element to the set with the current UTC timestamp.
pub fn (mut s LWWESet[T]) add(elem T) {
	s.add_with_time(elem, time.utc().unix_nano())
}

// add_with_time adds an element to the set with a specified timestamp in nanoseconds.
pub fn (mut s LWWESet[T]) add_with_time(elem T, ts i64) {
	if elem !in s.add_map || s.add_map[elem] < ts {
		s.add_map[elem] = ts
	}
}

// remove marks an element as removed with the current UTC timestamp.
pub fn (mut s LWWESet[T]) remove(elem T) {
	s.remove_with_time(elem, time.utc().unix_nano())
}

// remove_with_time marks an element as removed with a specified timestamp in nanoseconds.
pub fn (mut s LWWESet[T]) remove_with_time(elem T, ts i64) {
	if elem !in s.rm_map || s.rm_map[elem] < ts {
		s.rm_map[elem] = ts
	}
}

// lookup checks whether an element is currently a member of the set.
pub fn (s &LWWESet[T]) lookup(elem T) bool {
	if elem in s.add_map {
		if elem in s.rm_map {
			add_ts := s.add_map[elem]
			rm_ts := s.rm_map[elem]
			if add_ts > rm_ts {
				return true
			}
			if add_ts < rm_ts {
				return false
			}
			return s.bias == .add_wins
		}
		return true
	}
	return false
}

// elements returns an array of all active elements currently belonging to the set.
pub fn (s &LWWESet[T]) elements() []T {
	mut res := []T{}
	for elem, _ in s.add_map {
		if s.lookup(elem) {
			res << elem
		}
	}
	return res
}

// len returns the count of active elements currently belonging to the set.
pub fn (s &LWWESet[T]) len() int {
	return s.elements().len
}

// == checks whether two LWWESets have identical state and configuration.
pub fn (l LWWESet[T]) == (r LWWESet[T]) bool {
	if l.bias != r.bias || l.add_map.len != r.add_map.len || l.rm_map.len != r.rm_map.len {
		return false
	}
	for k, v in l.add_map {
		if k !in r.add_map || r.add_map[k] != v {
			return false
		}
	}
	for k, v in l.rm_map {
		if k !in r.rm_map || r.rm_map[k] != v {
			return false
		}
	}
	return true
}

// merge combines state from another LWWESet, taking the maximum timestamp for each element.
pub fn (mut s LWWESet[T]) merge(other &LWWESet[T]) {
	for elem, ts in other.add_map {
		if elem !in s.add_map || s.add_map[elem] < ts {
			s.add_map[elem] = ts
		}
	}
	for elem, ts in other.rm_map {
		if elem !in s.rm_map || s.rm_map[elem] < ts {
			s.rm_map[elem] = ts
		}
	}
}

