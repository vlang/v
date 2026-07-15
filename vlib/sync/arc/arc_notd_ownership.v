module arc

// Arc provides shared ownership of a heap-allocated value of type `T`.
// Cloning an Arc increments its atomic strong count instead of cloning `T`.
pub struct Arc[T] {
	inner &ArcInner[T]
}

// get returns a shared borrow of the value stored in this Arc.
pub fn (a &Arc[T]) get() &T {
	assert !isnil(a.inner)
	return &a.inner.value
}
