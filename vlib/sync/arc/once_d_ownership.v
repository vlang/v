module arc

import sync

// Once owns a value that is initialized at most once. Its clones share the cell.
@[noinit]
pub struct Once[T] implements IClone, Drop {
mut:
	inner Arc[OnceValue[T]]
}

struct OnceValue[T] implements Drop {
mut:
	mutex       &sync.Mutex
	initialized bool
	value       T
}

fn (mut cell OnceValue[T]) drop() {
	if cell.initialized {
		drop_owned(cell.value)
	}
	cell.mutex.destroy()
	// The last owner releases the allocation created by sync.new_mutex.
	unsafe { free(cell.mutex) }
}

// new_once returns an empty once-initialized cell.
pub fn new_once[T]() Once[T] {
	return Once[T]{
		inner: new(OnceValue[T]{ mutex: sync.new_mutex() })
	}
}

// new_once_with returns a cell that owns the supplied value.
pub fn new_once_with[T](value T) Once[T] {
	return Once[T]{
		inner: new(OnceValue[T]{
			mutex:       sync.new_mutex()
			initialized: true
			value:       value
		})
	}
}

// clone returns another owner of the same cell.
pub fn (cell &Once[T]) clone() Once[T] {
	return Once[T]{ inner: cell.inner.clone() }
}

// get returns a shared borrow once another caller has initialized the cell.
pub fn (cell &^a Once[T]) get[^a]() ?&^a T {
	data := cell.inner.get()
	data.mutex.lock()
	defer { data.mutex.unlock() }
	if !data.initialized {
		return none
	}
	return &cell.inner.inner.value.value
}

// get_or_init returns a shared borrow, running initialize once while holding the lock.
// The initializer must finish before calling methods on the same cell.
pub fn (cell &^a Once[T]) get_or_init[^a](initialize fn () T) &^a T {
	// The mutex protects publication. After initialization the value stays in place.
	mut data := unsafe { cell.inner.get() }
	data.mutex.lock()
	defer { data.mutex.unlock() }
	if !data.initialized {
		data.value = initialize()
		data.initialized = true
	}
	return &cell.inner.inner.value.value
}

// drop releases one owner of the cell.
pub fn (mut cell Once[T]) drop() {
	cell.inner.drop()
}
