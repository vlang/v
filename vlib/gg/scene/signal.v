module scene

// Signal is a small typed observer bus, mirroring Godot's signals. Listeners
// are called in registration order.
pub struct Signal[T] {
mut:
	listeners []fn (T)
}

// new_signal returns an empty signal.
pub fn new_signal[T]() Signal[T] {
	return Signal[T]{}
}

// listen registers a callback.
pub fn (mut s Signal[T]) listen(callback fn (T)) {
	s.listeners << callback
}

// emit calls every registered listener with value.
pub fn (mut s Signal[T]) emit(value T) {
	for listener in s.listeners {
		listener(value)
	}
}

// len returns the number of registered listeners.
pub fn (s &Signal[T]) len() int {
	return s.listeners.len
}
