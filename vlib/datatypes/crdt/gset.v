module crdt

// Gset is a grow-only set.
struct GSet[T] {
mut:
	main_set map[T]T
}

// new_gset returns an instance of GSet.
fn new_gset[T]() GSet[T] {
	return GSet[T]{
		main_set: map[T]T{}
	}
}

// add lets you add an element to grow-only set.
fn (mut g GSet[T]) add(elem T) {
	g.main_set[elem] = T{}
}

// lookup returns true if an element exists within the
// set or false otherwise.
fn (mut g GSet[T]) lookup(elem T) bool {
	return elem in g.main_set
}

// len returns the no. of elements present within GSet.
fn (mut g GSet[T]) len() int {
	return g.main_set.len
}

// elements returns all the elements present in the set.
fn (mut g GSet[T]) elements() []T {
	mut elements := []T{}
	for _, element in g.main_set {
		elements << element
	}
	return elements
}

// compare returns true if both of of sets are same, false otherwise.
fn (mut g GSet[T]) compare(c GSet[T]) bool {
	return g == c
}

// merge function to merge the GSet object's payload with the argument's payload.
fn (mut g GSet[T]) merge(c GSet[T]) {
	for key, _ in c.main_set {
		if key !in g.main_set {
			g.main_set[key] = c.main_set[key]
		}
	}
}
