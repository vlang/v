module datatypes

pub struct Set<T> {
mut:
	elements []T
	idx      int
}

// str returns a string representation of the set
pub fn (set Set<T>) str() string {
	return set.elements.str()
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
		if !set.contains(i) {
			set.elements << i
		}
	}
}

// clone returns a copy of the set
pub fn (set Set<T>) clone() Set<T> {
	mut new_set := Set<T>{}
	new_set.add(...set.elements)
	return new_set
}

// remove removes the element from the set and returns it
pub fn (mut set Set<T>) remove(i T) ?T {
	if set.is_empty() || !set.contains(i) {
		return error("Set doesn't contain item")
	}
	idx := set.elements.index(i)
	item := set.elements[idx]
	set.elements.delete(idx)
	return item
}

// delete_if_exists removes the element from the set if it exists otherwise nothing changes
pub fn (mut set Set<T>) delete_if_exists(i T) {
	if set.is_empty() || !set.contains(i) {
		return
	}
	set.elements.delete(set.elements.index(i))
}

// contains determines if set includes item
pub fn (set Set<T>) contains(i T) bool {
	return set.elements.contains(i)
}

// difference returns the difference between this set and another. Will panic if type differs
pub fn (set Set<T>) difference(s Set<T>) ?Set<T> {
	if !set.type_match(s) {
		return error("Set types don't match")
	}
	mut diff := Set<T>{}
	for item in set {
		if !s.contains(item) {
			diff.add(item)
		}
	}
	return diff
}

// union_with returns a set with all elements from both sets. Will panic if type differs
pub fn (set Set<T>) union_with(s Set<T>) ?Set<T> {
	if !set.type_match(s) {
		return error("Set types don't match")
	}
	mut u := s.clone()
	u.add(...set.elements) // add won't add duplicates
	return u
}

// intersection returns the items that are in both sets. Will panic if type differs
pub fn (set Set<T>) intersection(s Set<T>) ?Set<T> {
	if !set.type_match(s) {
		return error("Set types don't match")
	}
	mut inter := Set<T>{}
	if set.len() > s.len() {
		for item in s {
			if set.contains(item) {
				inter.add(item)
			}
		}
	} else {
		for item in set {
			if s.contains(item) {
				inter.add(item)
			}
		}
	}
	return inter
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
	mut total := a.clone()
	total.add(...b.elements)
	return total
}

pub fn (a Set<T>) - (b Set<T>) Set<T> {
	mut new_set := a.clone()
	for e in b {
		new_set.delete_if_exists(e)
	}
	return new_set
}

// is_subset returns if every element of set is in other set
pub fn (set Set<T>) is_subset(s Set<T>) bool {
	if !set.type_match(s) {
		return false
	}
	for item in set {
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
	for item in s {
		if !set.contains(item) {
			return false
		}
	}
	return true
}

fn (mut iter Set<T>) next() ?T {
	if iter.idx >= iter.len() {
		return error('Out of bounds')
	}
	defer {
		iter.idx++
	}
	return iter.elements[iter.idx]
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
