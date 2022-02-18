module patterns

// Iterator pattern implementation.
//
// Iterator is a behavioral design pattern that
// lets you traverse elements of a collection
// without exposing its underlying representation (list, stack, tree, etc.).
interface Iterator<T> {
	// check if the is another element
	has_next() bool
	// return the next element
	next() T
}
