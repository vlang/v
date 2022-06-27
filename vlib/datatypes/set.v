module set

import rand

pub struct Set<T> {
	// maximum number of values that Set<T> can hold.
	capacity int
mut:
	elements []T
}

// creates a new, initially empty set structure.
pub fn create<T>() Set<T> {
	return Set<T>{}
}

// creates a new set structure, initially empty but capable of holding up to n elements.
pub fn create_with_capacity<T>(capacity int) Set<T> {
	return Set<T>{
		capacity: capacity
		elements: []T{cap: capacity}
	}
}

// checks the element is exists.
fn (mut set Set<T>) exists(element T) bool {
	return element in set.elements
}

// adds the element to set, if it is not present already.
fn (mut set Set<T>) add(element T) {
	if set.capacity > 0 {
		if set.size() == set.capacity {
			set.elements = set.elements.filter(it != set.elements[0])
		}
	}
	if element !in set.elements {
		set.elements << element
	}
}

// removes the element from set, if it is present.
fn (mut set Set<T>) remove(element T) {
	if element !in set.elements {
		error("Item doesn't exist in set")
	}
	elements := set.elements.filter(it != element)
	set.elements = elements
}

// returns an arbitrary element of set
fn (mut set Set<T>) pick() ?T {
	if set.is_empty() {
		error('Set is empty')
	}
	return set.elements[rand.intn(set.size())?]
}

// returns the set consisting of all elements except for the arbitrary element.
fn (mut set Set<T>) rest() ?[]T {
	if set.is_empty() {
		error('Set is empty')
	}
	element := set.pick() or { return error('Failed to retrieve an arbitrary element') }
	return set.elements.filter(it != element)
}

// returns an arbitrary element and deleting it from set.
fn (mut set Set<T>) pop() ?T {
	if set.is_empty() {
		error('Stack is empty')
	}
	element := set.pick() or { return error('Failed to retrieve an arbitrary element') }
	elements := set.elements.filter(it != element)
	set.elements = elements
	return element
}

// delete all elements of S.
fn (mut set Set<T>) clear() {
	set.elements = []
}

// returns the number of elements.
fn (mut set Set<T>) size() int {
	return set.elements.len
}

// checks whether the two given sets are equal (i.e. contain all and only the same elements).
fn (mut l Set<T>) equal(r Set<T>) bool {
	for element in r.elements {
		if element !in l.elements {
			return false
		}
	}
	for element in l.elements {
		if element !in r.elements {
			return false
		}
	}
	return true
}

// checks whether the set is empty.
fn (mut set Set<T>) is_empty() bool {
	return set.elements.len == 0
}
