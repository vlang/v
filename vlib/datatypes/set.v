module datatypes

pub struct Set<T> {
mut:
	elements map[T]T
}

// str returns a string representation of the set
pub fn (set Set<T>) str() string {
	return set.elements.keys().str()
}

// is_empty checks if the set is empty
pub fn (set Set<T>) is_empty() bool {
	return set.elements.len == 0
}

// len returns the length of the set
pub fn (set Set<T>) len() int {
	return set.elements.len
}

// add inserts an item(s) into the set if not in set
pub fn (mut set Set<T>) add(item ...T) {
	for i in item {
		set.elements[i] = i
	}
}

// clone returns a copy of the set
pub fn (set Set<T>) clone() Set<T> {
	mut result := Set<T>{}
	result.add(...set.elements.keys())
	return result
}

// delete removes the element from the set if it exists
pub fn (mut set Set<T>) delete(i T) {
	if set.is_empty() || !set.contains(i) {
		return
	}
	set.elements.delete(i)
}

// contains determines if set includes item
pub fn (set Set<T>) contains(i T) bool {
	return i in set.elements
}

// difference returns the difference between this set and another. Will panic if type differs
pub fn (set Set<T>) difference(s Set<T>) !Set<T> {
	if !set.type_match(s) {
		return error("Set types don't match")
	}
	mut result := Set<T>{}
	for item in set.elements.keys() {
		if !s.contains(item) {
			result.add(item)
		}
	}
	return result
}

// union_with returns a set with all elements from both sets. Will panic if type differs
pub fn (set Set<T>) union_with(s Set<T>) !Set<T> {
	if !set.type_match(s) {
		return error("Set types don't match")
	}
	mut result := s.clone()
	result.add(...set.elements.keys()) // add won't add duplicates
	return result
}

// intersection returns the items that are in both sets. Will panic if type differs
pub fn (set Set<T>) intersection(s Set<T>) !Set<T> {
	if !set.type_match(s) {
		return error("Set types don't match")
	}
	mut result := Set<T>{}
	if set.len() > s.len() {
		for item in s.elements.keys() {
			if set.contains(item) {
				result.add(item)
			}
		}
	} else {
		for item in set.elements.keys() {
			if s.contains(item) {
				result.add(item)
			}
		}
	}
	return result
}

// equal returns if two sets are equal
pub fn (set Set<T>) equal(s Set<T>) bool {
	intersect := set.intersection(s) or { Set<T>{} }
	if !set.type_match(s) || (set.len() != s.len() && intersect.len() != s.len()) {
		return false
	}
	return true
}

pub fn (a Set<T>) == (b Set<T>) bool {
	return a.equal(b)
}

pub fn (a Set<T>) + (b Set<T>) Set<T> {
	mut result := a.clone()
	result.add(...b.elements.keys())
	return result
}

pub fn (a Set<T>) - (b Set<T>) Set<T> {
	mut result := a.clone()
	for e in b.elements.keys() {
		result.delete(e)
	}
	return result
}

// is_subset returns if every element of set is in other set
pub fn (set Set<T>) is_subset(s Set<T>) bool {
	if !set.type_match(s) {
		return false
	}
	for item in set.elements.keys() {
		if !s.contains(item) {
			return false
		}
	}
	return true
}

// is_superset returns if every element of other set is in this set
pub fn (set Set<T>) is_superset(s Set<T>) bool {
	if !set.type_match(s) {
		return false
	}
	for item in s.elements.keys() {
		if !set.contains(item) {
			return false
		}
	}
	return true
}

// is_proper_subset returns if every element of set is in other set, but not equal
pub fn (set Set<T>) is_proper_subset(s Set<T>) bool {
	return set != s && set.is_subset(s)
}

// is_proper_superset returns if every element of other set is in this set, but not equal
pub fn (set Set<T>) is_proper_superset(s Set<T>) bool {
	return set != s && set.is_superset(s)
}

fn (set Set<T>) type_match(s Set<T>) bool {
	return typeof(set).name == typeof(s).name
}
