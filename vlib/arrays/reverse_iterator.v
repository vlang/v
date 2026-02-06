module arrays

// ReverseIterator provides a convenient way to iterate in reverse over all elements of an array without allocations.
// I.e. it allows you to use this syntax: `for elem in arrays.reverse_iterator(a) {` .
pub struct ReverseIterator[T] {
mut:
	a []T
	i int
}

// reverse_iterator can be used to iterate over the elements in an array.
// i.e. you can use this syntax: `for elem in arrays.reverse_iterator(a) {` .
pub fn reverse_iterator[T](a []T) ReverseIterator[T] {
	return ReverseIterator[T]{
		a: a
		i: a.len
	}
}

// next is the required method, to implement an iterator in V.
// It returns none when the iteration should stop.
// Otherwise it returns the current element of the array.
@[direct_array_access]
pub fn (mut iter ReverseIterator[T]) next() ?&T {
	iter.i--
	if iter.i < 0 {
		return none
	}
	return unsafe { &iter.a[iter.i] }
}

// free frees the iterator resources.
pub fn (iter &ReverseIterator[T]) free() {
	// The array stored in the iterator is not owned by the iterator.
	// It should not be freed, when the iterator goes out of scope.
	// This is the reason, that this manual free method exists and is empty.
}
