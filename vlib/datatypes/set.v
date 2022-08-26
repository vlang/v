module datatypes

pub struct Set<T> {
mut:
	elements map[T]u8
}

// checks the element is exists.
pub fn (set Set<T>) exists(element T) bool {
	return element in set.elements
}

// adds the element to set, if it is not present already.
pub fn (mut set Set<T>) add(element T) {
	set.elements[element] = 1
}

// removes the element from set.
pub fn (mut set Set<T>) remove(element T) {
	set.elements.delete(element)
}

// pick returns an arbitrary element of set, if set is not empty.
pub fn (set Set<T>) pick() ?T {
	for k, _ in set.elements {
		return k
	}
	return error('Set is empty.')
}

// rest returns the set consisting of all elements except for the arbitrary element.
pub fn (mut set Set<T>) rest() ?[]T {
	element := set.pick()?
	return set.elements.keys().filter(it != element)
}

// pop returns an arbitrary element and deleting it from set.
pub fn (mut set Set<T>) pop() ?T {
	element := set.pick()?
	set.elements.delete(element)
	return element
}

// delete all elements of set.
pub fn (mut set Set<T>) clear() {
	set.elements = map[T]u8{}
}

// equal checks whether the two given sets are equal (i.e. contain all and only the same elements).
[deprecated: 'use set1<T> == set2<T> instead']
pub fn (l Set<T>) equal(r Set<T>) bool {
	return l == r
}

// == checks whether the two given sets are equal (i.e. contain all and only the same elements).
pub fn (l Set<T>) == (r Set<T>) bool {
	if l.elements.len != r.elements.len {
		return false
	}
	for e, _ in r.elements {
		if e !in l.elements {
			return false
		}
	}
	return true
}

// is_empty checks whether the set is empty or not.
pub fn (set Set<T>) is_empty() bool {
	return set.size() == 0
}

// size returns the number of elements in the set.
pub fn (set Set<T>) size() int {
	return set.elements.len
}

// copy returns a copy of all the elements in the set.
pub fn (set Set<T>) copy() Set<T> {
	return Set<T>{
		elements: set.elements.clone()
	}
}

// add_all adds the whole `elements` array to the set
pub fn (mut set Set<T>) add_all(elements []T) {
	for element in elements {
		set.add(element)
	}
}

// @union returns the union of the two sets.
pub fn (l Set<T>) @union(r Set<T>) Set<T> {
	mut set := l
	for e, _ in r.elements {
		set.add(e)
	}
	return set
}

// intersection returns the intersection of sets.
pub fn (l Set<T>) intersection(r Set<T>) Set<T> {
	mut set := l
	for e, _ in l.elements {
		if !r.exists(e) {
			set.remove(e)
		}
	}
	for e, _ in r.elements {
		if !l.exists(e) {
			set.remove(e)
		}
	}
	return set
}

// difference returns the difference of sets.
[deprecated: 'use set1<T> - set2<T> instead']
pub fn (l Set<T>) difference(r Set<T>) Set<T> {
	return l - r
}

// - returns the difference of sets.
pub fn (l Set<T>) - (r Set<T>) Set<T> {
	mut set := l
	for e, _ in l.elements {
		if r.exists(e) {
			set.remove(e)
		}
	}
	return set
}

// subset returns true if the set `r` is a subset of the set `l`.
pub fn (l Set<T>) subset(r Set<T>) bool {
	for e, _ in r.elements {
		if e !in l.elements {
			return false
		}
	}
	return true
}
