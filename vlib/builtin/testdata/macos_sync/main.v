module main

import sync

struct Counter {
mut:
	lock  sync.Mutex
	value int
}

struct RwProbe {
mut:
	lock  sync.RwMutex
	value int
}

fn require_sync(ok bool) {
	if !ok { exit(93) }
}

fn increment(mut counter Counter) {
	for _ in 0 .. 5000 {
		counter.lock.lock()
		counter.value++
		counter.lock.unlock()
	}
}

fn publish(mut sem sync.Semaphore) {
	for _ in 0 .. 2000 { sem.post() }
}

fn consume(mut sem sync.Semaphore) {
	for _ in 0 .. 1000 { sem.wait() }
}

fn write_one(mut value RwProbe) {
	value.lock.lock()
	require_sync(value.value == 0)
	value.value = 1
	value.lock.unlock()
}

fn read_one(mut value RwProbe) bool {
	value.lock.rlock()
	result := value.value == 1
	value.lock.runlock()
	return result
}

fn main() {
	mut counter := &Counter{}
	mut increments := []thread{}
	for _ in 0 .. 4 { increments << spawn increment(mut counter) }
	increments.wait()
	require_sync(counter.value == 20000)
	require_sync(counter.lock.try_lock())
	require_sync(!counter.lock.try_lock())
	counter.lock.unlock()
	counter.lock.destroy()
	mut sem := &sync.Semaphore{}
	require_sync(!sem.try_wait() && !sem.timed_wait(0))
	start, error := bf_monotonic_now()
	require_sync(error == 0 && !sem.timed_wait(2_000_000))
	stop, stop_error := bf_monotonic_now()
	require_sync(stop_error == 0 && stop - start >= 2_000_000)
	mut consumers := []thread{}
	for _ in 0 .. 4 { consumers << spawn consume(mut sem) }
	first := spawn publish(mut sem)
	second := spawn publish(mut sem)
	first.wait()
	second.wait()
	consumers.wait()
	require_sync(!sem.try_wait())
	sem.post()
	require_sync(sem.timed_wait(0) && !sem.try_wait())
	sem.destroy()
	mut rw := &RwProbe{}
	rw.lock.rlock()
	writer := spawn write_one(mut rw)
	// While the initial read lock is held, only this thread and the writer
	// touch metadata. Failure means the writer is in the gate or registered;
	// the subsequently created reader must therefore wait behind that writer.
	for rw.lock.try_rlock() { rw.lock.runlock() }
	reader := spawn read_one(mut rw)
	rw.lock.runlock()
	writer.wait()
	require_sync(reader.wait())
	require_sync(rw.lock.try_wlock())
	require_sync(!rw.lock.try_rlock() && !rw.lock.try_wlock())
	rw.lock.unlock()
	rw.lock.destroy()
	println('raw macOS synchronization ok')
}
