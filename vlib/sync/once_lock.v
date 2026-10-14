module sync

// OnceLock is a cell that can be written to at most once and is safe to share
// between threads. It is the V-idiomatic equivalent of Rust's
// `std::sync::OnceLock`: `get_or_init` runs the initializer exactly once no
// matter how many threads race on it. Returned reference and container values
// can still alias the stored value; callers must synchronize later mutations.
// A OnceLock must be created with
// `new_once_lock`, since the internal mutex needs initialization.
@[heap]
pub struct OnceLock[T] {
mut:
	mutex &Mutex
	done  bool
	value T
}

// new_once_lock creates an empty (uninitialised) OnceLock.
pub fn new_once_lock[T]() &OnceLock[T] {
	mut m := new_mutex()
	return &OnceLock[T]{
		mutex: m
	}
}

// get returns the stored value, or `none` if the cell is not initialised yet.
// If an initializer is running, get waits for it to finish.
pub fn (o &OnceLock[T]) get() ?T {
	o.mutex.lock()
	defer {
		o.mutex.unlock()
	}
	if o.done {
		return o.value
	}
	return none
}

// set stores `value` and returns `true`, unless the cell was already
// initialised — in that case the stored value is kept and `false` is returned.
pub fn (mut o OnceLock[T]) set(value T) bool {
	o.mutex.lock()
	defer {
		o.mutex.unlock()
	}
	if o.done {
		return false
	}
	o.value = value
	o.done = true
	return true
}

// get_or_init returns the stored value, running `f` to initialise the cell on
// the first call. Concurrent callers block until the initialisation finishes
// and all observe the same value.
// Note: the initializer runs under the cell's mutex. If `f` calls `get`,
// `set`, or `get_or_init` on the same cell, it will deadlock.
pub fn (mut o OnceLock[T]) get_or_init(f fn () T) T {
	o.mutex.lock()
	defer {
		o.mutex.unlock()
	}
	if !o.done {
		o.value = f()
		o.done = true
	}
	return o.value
}

// LazyLock is a value that is computed on first access and is safe to share
// between threads. It is the V-idiomatic equivalent of Rust's
// `std::sync::LazyLock`.
@[heap]
pub struct LazyLock[T] {
mut:
	inner &OnceLock[T]
	init  fn () T
}

// new_lazy_lock creates a lazy value computed by `f` on first `get`.
pub fn new_lazy_lock[T](f fn () T) &LazyLock[T] {
	return &LazyLock[T]{
		inner: new_once_lock[T]()
		init:  f
	}
}

// get returns the value, running the initializer once on the first call.
// Concurrent callers block until the initialisation finishes.
// The initializer must not call get on the same LazyLock: that would deadlock.
pub fn (mut l LazyLock[T]) get() T {
	return l.inner.get_or_init(l.init)
}
