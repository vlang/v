module crdt

// GSet represents a Grow-only Set (G-Set) in CRDT.
// Elements can only be added to the set; removals are not supported.
// Merging two G-Sets performs a mathematical set union.
pub struct GSet[T] {
mut:
	main_set map[T]u8
}

// new_gset creates a new empty GSet.
pub fn new_gset[T]() GSet[T] {
	return GSet[T]{
		main_set: map[T]u8{}
	}
}

// add adds an element to the GSet.
pub fn (mut g GSet[T]) add(elem T) {
	g.main_set[elem] = 1
}

// lookup returns true if the element exists in the set, false otherwise.
pub fn (g &GSet[T]) lookup(elem T) bool {
	return elem in g.main_set
}

// len returns the number of elements in the set.
pub fn (g &GSet[T]) len() int {
	return g.main_set.len
}

// elements returns an array of all elements present in the set.
pub fn (g &GSet[T]) elements() []T {
	return g.main_set.keys()
}

// == checks whether two GSets contain the exact same elements.
pub fn (l GSet[T]) == (r GSet[T]) bool {
	if l.main_set.len != r.main_set.len {
		return false
	}
	for key, _ in r.main_set {
		if key !in l.main_set {
			return false
		}
	}
	return true
}

// merge combines elements from another GSet into this set via set union.
pub fn (mut g GSet[T]) merge(other &GSet[T]) {
	for key, _ in other.main_set {
		g.main_set[key] = 1
	}
}

