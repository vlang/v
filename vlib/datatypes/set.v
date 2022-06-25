module datatypes

pub struct Set<T> {
mut:
	elements []T
}

fn (mut set Set<T>) exists(element T) bool {
	return element in set.elements
}

fn (mut set Set<T>) add(element T) {
	if element !in set.elements {
		set.elements << element
	}
}

fn (mut set Set<T>) remove(element T) {
	if element !in set.elements {
		error("remove error: Item doesn't exist in set")
	}
	elements := set.elements.filter(it != element)
	set.elements = elements
}

fn (mut set Set<T>) size() int {
	return set.elements.len
}
