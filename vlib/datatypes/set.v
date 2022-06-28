module datatypes

pub struct Set<T> {
mut:
	elements map[T]u8
}

// checks the element is exists.
fn (set Set<T>) exists(element T) bool {
	return element in set.elements
}

// adds the element to set, if it is not present already.
fn (mut set Set<T>) add(element T) {
	if element !in set.elements {
		set.elements[element] = u8(set.size() + 1)
	}
}

// removes the element from set.
fn (mut set Set<T>) remove(element T) {
	set.elements.delete(element)
}

// returns an arbitrary element of set, if set is not empty.
fn (mut set Set<T>) pick() ?T {
	for k, _ in set.elements {
		return k
	}
	return error('Set is empty.')
}

// returns the set consisting of all elements except for the arbitrary element.
fn (mut set Set<T>) rest() ?[]T {
	element := set.pick()?
	return set.elements.keys().filter(it != element)
}

// returns an arbitrary element and deleting it from set.
fn (mut set Set<T>) pop() ?T {
	element := set.pick()?
	set.elements.delete(element)
	return element
}

// delete all elements of set.
fn (mut set Set<T>) clear() {
	set.elements = map[T]u8{}
}

// checks whether the two given sets are equal (i.e. contain all and only the same elements).
fn (mut l Set<T>) equal(r Set<T>) bool {
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

// checks whether the set is empty.
fn (mut set Set<T>) is_empty() bool {
	return set.size() == 0
}

// returns the number of elements.
fn (mut set Set<T>) size() int {
	return set.elements.len
}

// returns a copy of the set.
fn (mut set Set<T>) copy() Set<T> {
	return Set<T>{
		elements: set.elements.clone()
	}
}

// updates the set from the array
fn (mut set Set<T>) update(elements []T) {
	for element in elements {
		set.add(element)
	}
}

// returns the union of sets.
fn (mut l Set<T>) union_(r Set<T>) Set<T> {
	mut set := l
	for e, _ in r.elements {
		set.add(e)
	}
	return set
}

// returns the intersection of sets.
fn (mut l Set<T>) intersection(r Set<T>) Set<T> {
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

// returns the difference of sets.
fn (mut l Set<T>) difference(r Set<T>) Set<T> {
	mut set := l
	for e, _ in l.elements {
		if r.exists(e) {
			set.remove(e)
		}
	}
	return set
}

// returns true if the set is a subset.
fn (mut l Set<T>) subset(r Set<T>) bool {
	for e, _ in r.elements {
		if e !in l.elements {
			return false
		}
	}
	return true
}
