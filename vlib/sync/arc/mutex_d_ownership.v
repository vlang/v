module arc

import sync

// Mutex owns a protected value. Its clones share the value and lock.
pub struct Mutex[T] implements IClone, Drop {
mut:
	inner Arc[MutexValue[T]]
}

// MutexUpdate supplies the replacement value and the result of an update.
pub struct MutexUpdate[T, R] {
pub:
	value  T
	result R
}

struct MutexPayload[T] {
	value T
}

struct MutexValue[T] implements Drop {
mut:
	mutex &sync.Mutex
	value ?MutexPayload[T]
}

fn (mut m MutexValue[T]) drop() {
	drop_owned(m.value)
	m.mutex.destroy()
	// The last Arc owner destroys the native mutex and releases its allocation.
	unsafe { free(m.mutex) }
}

// new_mutex moves value into a reference-counted mutex.
pub fn new_mutex[T](value T) Mutex[T] {
	return Mutex[T]{
		inner: new(MutexValue[T]{
			mutex: sync.new_mutex()
			value: MutexPayload[T]{ value: value }
		})
	}
}

// clone returns another owner of the same protected value.
pub fn (m &Mutex[T]) clone() Mutex[T] {
	return Mutex[T]{ inner: m.inner.clone() }
}

// drop releases one owner of the protected value.
pub fn (mut m Mutex[T]) drop() {
	m.inner.drop()
}

// update calls work with ownership of the protected value while holding the lock.
// Work returns the replacement value and a result. It must not acquire the same lock.
pub fn (m &Mutex[T]) update[R](work fn (T) MutexUpdate[T, R]) R {
	mut data := unsafe { m.inner.get() }
	data.mutex.lock()
	defer {
		data.mutex.unlock()
	}
	mut old := ?MutexPayload[T](none)
	// Move the optional payload to local storage without dropping it.
	// The empty option replaces it before the callback can access the value.
	unsafe {
		empty := ?MutexPayload[T](none)
		vmemcpy(&old, &data.value, sizeof(?MutexPayload[T]))
		vmemcpy(&data.value, &empty, sizeof(?MutexPayload[T]))
	}
	payload := old or { panic('arc.Mutex has no protected value') }
	updated := work(payload.value)
	data.value = MutexPayload[T]{ value: updated.value }
	return updated.result
}
