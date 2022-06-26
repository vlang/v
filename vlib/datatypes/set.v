module datatypes

import rand

pub struct Set<T> {
mut:
	elements []T
}

/*
// checks whether the set is empty.
fn (mut set Set<T>) is_empty(s Set<T>) bool {
	return s.elements.len == 0
}

fn (mut set Set<T>) is_element_of(element T, s Set<T>) bool {
	element in s.elements
}
*/

// checks the element is exists.
fn (mut set Set<T>) exists(element T) bool {
	return element in set.elements
}

// adds the element to set, if it is not present already.
fn (mut set Set<T>) add(element T) {
	if element !in set.elements {
		set.elements << element
	}
}

// removes the element from set, if it is present.
fn (mut set Set<T>) remove(element T) {
	if element !in set.elements {
		error("remove error: Item doesn't exist in set")
	}
	elements := set.elements.filter(it != element)
	set.elements = elements
}

// returns an arbitrary element of set
fn (mut set Set<T>) pick() ?T {
	if set.elements.len == 0 {
		error("pick error: It's not possible to get element from an empty set")
	}
	return set.elements[rand.intn(set.size())?]
}

// returns the set consisting of all elements except for the arbitrary element.
fn (mut set Set<T>) rest() ?[]T {
	if set.elements.len == 0 {
		error("rest error: It's not possible to remove element from an empty set")
	}
	element := set.pick() or { panic('vlib/datatypes/set/rest') }
	return set.elements.filter(it != element)
}

// returns an arbitrary element and deleting it from set.
fn (mut set Set<T>) pop() ?T {
	if set.elements.len == 0 {
		error("pop error: It's not possible to remove element from an empty set")
	}
	element := set.pick() or { panic('vlib/datatypes/set/pop') }
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
