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

fn (mut data MutexValue[T]) take_value() T {
	mut old := ?MutexPayload[T](none)
	// Move the optional payload to local storage while the mutex is held.
	unsafe {
		empty := ?MutexPayload[T](none)
		vmemcpy(&old, &data.value, sizeof(?MutexPayload[T]))
		vmemcpy(&data.value, &empty, sizeof(?MutexPayload[T]))
	}
	payload := old or { panic('arc.Mutex has no protected value') }
	return payload.value
}

// update calls work with ownership of the protected value while holding the lock.
// Work returns the replacement value and a result. It must not acquire the same lock.
pub fn (m &Mutex[T]) update[R](work fn (T) MutexUpdate[T, R]) R {
	mut data := unsafe { m.inner.get() }
	data.mutex.lock()
	defer { data.mutex.unlock() }
	next := work(data.take_value())
	data.value = MutexPayload[T]{ value: next.value }
	return next.result
}

// update_with moves context into a protected update and returns the result.
// Work receives the value and context while the lock is held.
pub fn (m &Mutex[T]) update_with[C, R](context C, work fn (T, C) MutexUpdate[T, R]) R {
	mut data := unsafe { m.inner.get() }
	data.mutex.lock()
	defer { data.mutex.unlock() }
	next := work(data.take_value(), context)
	data.value = MutexPayload[T]{ value: next.value }
	return next.result
}

// MutexPairUpdate supplies both replacement values and a result.
pub struct MutexPairUpdate[T, R] {
pub:
	first  T
	second T
	result R
}

// update_pair moves two protected values into one update while holding both locks.
// The mutexes must be distinct. Work must not acquire either lock again.
pub fn (m &Mutex[T]) update_pair[R](other &Mutex[T], work fn (T, T) MutexPairUpdate[T, R]) R {
	mut first := unsafe { m.inner.get() }
	mut second := unsafe { other.inner.get() }
	first_address := u64(voidptr(first))
	second_address := u64(voidptr(second))
	if first_address == second_address {
		panic('arc.Mutex.update_pair requires distinct mutexes')
	}
	// All callers use the same acquisition order, including reversed pairs.
	if first_address < second_address {
		first.mutex.lock()
		second.mutex.lock()
	} else {
		second.mutex.lock()
		first.mutex.lock()
	}
	defer {
		first.mutex.unlock()
		second.mutex.unlock()
	}
	next := work(first.take_value(), second.take_value())
	first.value = MutexPayload[T]{ value: next.first }
	second.value = MutexPayload[T]{ value: next.second }
	return next.result
}
