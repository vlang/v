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
	set.elements = map[T]int{}
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
