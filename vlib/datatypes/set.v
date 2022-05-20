module datatypes

pub struct Set<T> {
mut:
	elements map[T]bool
}

// str returns a string representation of the set
pub fn (set Set<T>) str() string {
	return set.elements.keys().str()
}

// is_empty checks if the set is empty
pub fn (set Set<T>) is_empty() bool {
	return set.elements.keys().len == 0
}

// len returns the length of the set
pub fn (set Set<T>) len() int {
	return set.elements.keys().len
}

// add inserts an item(s) into the set if not in set
pub fn (mut set Set<T>) add(item ...T) {
	for i in item {
		set.elements[i] = true
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
	return set.elements.keys().index(i)>=0
}

// difference returns the difference between this set and another. Will panic if type differs
pub fn (s1 Set<T>) difference(s2 Set<T>) !Set<T> {
	if !s1.type_match(s2) {
		return error("Set types don't match")
	}
	mut result := Set<T>{}
	for item in s1 .elements.keys() {
		if !s2.contains(item) {
			result.add(item)
		}
	}
	return result
}

// union_with returns a set with all elements from both sets. Will panic if type differs
pub fn (s1 Set<T>) union_with(s2 Set<T>) !Set<T> {
	if !s1.type_match(s2) {
		return error("Set types don't match")
	}
	mut result := s2.clone()
	result.add(...s1.elements.keys()) // add won't add duplicates
	return result
}

// intersection returns the items that are in both sets. Will panic if type differs
pub fn (s1 Set<T>) intersection(s2 Set<T>) !Set<T> {
	if !s1.type_match(s2) {
		return error("Set types don't match")
	}
	mut result := Set<T>{}
	if s1.len() > s2.len() {
		for item in s2.elements.keys() {
			if s1.contains(item) {
				result.add(item)
			}
		}
	} else {
		for item in s1.elements.keys() {
			if s2.contains(item) {
				result.add(item)
			}
		}
	}
	return result
}

// equal returns if two sets are equal
pub fn (s1 Set<T>) equal(s2 Set<T>) bool {
	if !s1.type_match(s2) {
		return false
	}
	intersect := s1.intersection(s2) or { Set<T>{} }
	if s1.len() != s2.len() && intersect.len() != s2.len() {
		return false
	}
	return true
}

pub fn (s1 Set<T>) == (s2 Set<T>) bool {
	return s1.equal(s2)
}

pub fn (s1 Set<T>) + (s2 Set<T>) Set<T> {
	mut result := s1.clone()
	result.add(...s2.elements.keys())
	return result
}

pub fn (s1 Set<T>) - (s2 Set<T>) Set<T> {
	mut result := s1.clone()
	for e in s2.elements.keys() {
		result.delete(e)
	}
	return result
}

// is_subset returns if every element of set is in other set
pub fn (s1 Set<T>) is_subset(s2 Set<T>) bool {
	if !s1.type_match(s2) {
		return false
	}
	for item in s1.elements.keys() {
		if !s2.contains(item) {
			return false
		}
	}
	return true
}

// is_superset returns if every element of other set is in this set
pub fn (s1 Set<T>) is_superset(s2 Set<T>) bool {
	if !s1.type_match(s2) {
		return false
	}
	for item in s2.elements.keys() {
		if !s1.contains(item) {
			return false
		}
	}
	return true
}

// is_proper_subset returns if every element of set is in other set, but not equal
pub fn (s1 Set<T>) is_proper_subset(s2 Set<T>) bool {
	return s1 != s2 && s1.is_subset(s2)
}

// is_proper_superset returns if every element of other set is in this set, but not equal
pub fn (s1 Set<T>) is_proper_superset(s2 Set<T>) bool {
	return s1 != s2 && s1.is_superset(s2)
}

fn (s1 Set<T>) type_match(s2 Set<T>) bool {
	return typeof(s1).name == typeof(s2).name
}
