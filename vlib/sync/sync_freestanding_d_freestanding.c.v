module sync

$if (linux || (macos && amd64)) && !native && !i386 {
	fn C.atomic_load_u32(voidptr) u32
	fn C.atomic_store_u32(voidptr, u32)
	fn C.atomic_compare_exchange_strong_u32(voidptr, voidptr, u32) bool
	fn C.atomic_exchange_u32(voidptr, u32) u32
	fn C.atomic_fetch_add_u32(voidptr, u32) u32
	fn C.atomic_fetch_sub_u32(voidptr, u32) u32

	// Zero-value locks and semaphores are usable. A contended operation parks
	// through a private kernel futex or ULock; no pthread object is constructed.
	@[heap]
	pub struct Mutex {
	mut:
		state u32 // 0 unlocked, 1 locked, 2 locked with possible waiters
	}

	@[heap]
	pub struct RwMutex {
	mut:
		gate            Mutex
		readers         u32
		waiting_readers u32
		waiting_writers u32
		writer          bool
		// At every gate release these words describe the actual blocking
		// predicates, so an ABA can repeat only while a caller is blocked again.
		read_blocked  u32
		write_blocked u32
	}

	@[heap]
	pub struct Semaphore {
	mut:
		count   u32
		waiters u32
	}

	fn wait_word_error(word &u32, expected u32, timeout i64) int {
		$if macos {
			// XNU's ULock copyin may fail on an untouched anonymous page.
			// The atomic predicate read also avoids parking after a state change.
			if C.atomic_load_u32(word) != expected { return 11 }
			return bf_wait_u32(word, expected, timeout)
		} $else {
			return bf_futex_wait(word, expected, timeout)
		}
	}

	fn wait_word(word &u32, expected u32) {
		code := wait_word_error(word, expected, -1)
		if code !in [0, 4, 11] { // success, interrupted, value changed
			cpanic(code)
		}
	}

	fn wake_word(word &u32, count int) {
		$if macos {
			code := bf_wake_u32(word, count > 1)
			if code != 0 { cpanic(code) }
		} $else {
			_, code := bf_futex_wake(word, count)
			if code != 0 { cpanic(code) }
		}
	}

	// new_mutex creates and initializes an unlocked mutex.
	pub fn new_mutex() &Mutex {
		return &Mutex{}
	}

	// init initializes an unlocked mutex before its first use or reuse.
	pub fn (mut m Mutex) init() {
		C.atomic_store_u32(&m.state, 0)
	}

	// lock acquires the mutex, parking in the kernel when it is contended.
	pub fn (mut m Mutex) lock() {
		if m.try_lock() {
			return
		}
		for C.atomic_exchange_u32(&m.state, 2) != 0 {
			wait_word(&m.state, 2)
		}
	}

	// try_lock acquires an unlocked mutex without waiting.
	pub fn (mut m Mutex) try_lock() bool {
		mut expected := u32(0)
		return C.atomic_compare_exchange_strong_u32(&m.state, &expected, 1)
	}

	// unlock releases the mutex and wakes a waiter when it was contended.
	pub fn (mut m Mutex) unlock() {
		if C.atomic_exchange_u32(&m.state, 0) == 2 {
			wake_word(&m.state, 1)
		}
	}

	// destroy validates that the mutex is unlocked. It owns no kernel handle.
	pub fn (mut m Mutex) destroy() {
		if C.atomic_load_u32(&m.state) != 0 {
			panic('sync: destroying a locked mutex')
		}
	}

	// new_rwmutex creates an unlocked reader/writer mutex.
	pub fn new_rwmutex() &RwMutex {
		return &RwMutex{}
	}

	// init initializes a reader/writer mutex before its first use or reuse.
	pub fn (mut m RwMutex) init() {
		m.gate.init()
		m.readers = 0
		m.waiting_readers = 0
		m.waiting_writers = 0
		m.writer = false
		C.atomic_store_u32(&m.read_blocked, 0)
		C.atomic_store_u32(&m.write_blocked, 0)
	}

	// rlock acquires a read lock. Queued writers take priority over new readers.
	pub fn (mut m RwMutex) rlock() {
		m.gate.lock()
		if m.writer || m.waiting_writers != 0 {
			m.waiting_readers++
			for m.writer || m.waiting_writers != 0 {
				m.gate.unlock()
				wait_word(&m.read_blocked, 1)
				m.gate.lock()
			}
			m.waiting_readers--
		}
		if m.readers == 0xffffffff {
			m.gate.unlock()
			panic('sync: reader count overflow')
		}
		m.readers++
		if m.readers == 1 {
			C.atomic_store_u32(&m.write_blocked, 1)
		}
		m.gate.unlock()
	}

	// lock acquires an exclusive write lock.
	pub fn (mut m RwMutex) lock() {
		m.gate.lock()
		m.waiting_writers++
		C.atomic_store_u32(&m.read_blocked, 1)
		for m.writer || m.readers != 0 {
			m.gate.unlock()
			wait_word(&m.write_blocked, 1)
			m.gate.lock()
		}
		m.waiting_writers--
		m.writer = true
		C.atomic_store_u32(&m.write_blocked, 1)
		m.gate.unlock()
	}

	// try_rlock acquires a read lock without waiting.
	pub fn (mut m RwMutex) try_rlock() bool {
		if !m.gate.try_lock() {
			return false
		}
		acquired := !m.writer && m.waiting_writers == 0 && m.readers < 0xffffffff
		if acquired {
			m.readers++
			if m.readers == 1 {
				C.atomic_store_u32(&m.write_blocked, 1)
			}
		}
		m.gate.unlock()
		return acquired
	}

	// try_wlock acquires a write lock without waiting.
	pub fn (mut m RwMutex) try_wlock() bool {
		if !m.gate.try_lock() {
			return false
		}
		acquired := !m.writer && m.readers == 0
		if acquired {
			m.writer = true
			C.atomic_store_u32(&m.read_blocked, 1)
			C.atomic_store_u32(&m.write_blocked, 1)
		}
		m.gate.unlock()
		return acquired
	}

	// runlock releases a read lock and wakes queued writers after the last reader.
	pub fn (mut m RwMutex) runlock() {
		m.gate.lock()
		if m.readers == 0 {
			m.gate.unlock()
			panic('sync: unlocking an unlocked read mutex')
		}
		m.readers--
		wake_writer := m.readers == 0 && m.waiting_writers != 0
		if m.readers == 0 {
			C.atomic_store_u32(&m.write_blocked, 0)
		}
		m.gate.unlock()
		if wake_writer {
			wake_word(&m.write_blocked, 1)
		}
	}

	// unlock releases a write lock and wakes blocked readers and writers.
	pub fn (mut m RwMutex) unlock() {
		m.gate.lock()
		if !m.writer {
			m.gate.unlock()
			panic('sync: unlocking an unlocked write mutex')
		}
		m.writer = false
		C.atomic_store_u32(&m.write_blocked, 0)
		wake_writer := m.waiting_writers != 0
		wake_readers := !wake_writer && m.waiting_readers != 0
		if !wake_writer {
			C.atomic_store_u32(&m.read_blocked, 0)
		}
		m.gate.unlock()
		if wake_writer {
			wake_word(&m.write_blocked, 1)
		} else if wake_readers {
			wake_word(&m.read_blocked, 0x7fffffff)
		}
	}

	// destroy validates that no reader, writer or waiter uses the mutex.
	pub fn (mut m RwMutex) destroy() {
		m.gate.destroy()
		if m.readers != 0 || m.writer || m.waiting_readers != 0 || m.waiting_writers != 0 {
			panic('sync: destroying a locked read/write mutex')
		}
	}

	// new_semaphore creates a semaphore with an initial count of zero.
	pub fn new_semaphore() &Semaphore {
		return new_semaphore_init(0)
	}

	// new_semaphore_init creates a semaphore with an initial count of n.
	pub fn new_semaphore_init(n u32) &Semaphore {
		mut sem := &Semaphore{}
		sem.init(n)
		return sem
	}

	// init initializes a semaphore before its first use or reuse.
	pub fn (mut sem Semaphore) init(n u32) {
		C.atomic_store_u32(&sem.count, n)
		C.atomic_store_u32(&sem.waiters, 0)
	}

	// post adds one permit and wakes a waiting thread. Posting to a full u32
	// counter panics without wrapping its value.
	pub fn (mut sem Semaphore) post() {
		mut count := C.atomic_load_u32(&sem.count)
		for {
			if count == 0xffffffff {
				panic('sync: semaphore count overflow')
			}
			if C.atomic_compare_exchange_strong_u32(&sem.count, &count, count + 1) {
				break
			}
		}
		// Each post may release a different waiter even when an earlier permit
		// has not been consumed yet. Waking only on 0 -> 1 loses those wakeups.
		if C.atomic_load_u32(&sem.waiters) != 0 {
			wake_word(&sem.count, 1)
		}
	}

	// try_wait consumes a permit if one is available, without waiting.
	pub fn (mut sem Semaphore) try_wait() bool {
		mut count := C.atomic_load_u32(&sem.count)
		for count != 0 {
			if C.atomic_compare_exchange_strong_u32(&sem.count, &count, count - 1) {
				return true
			}
		}
		return false
	}

	// wait consumes a permit, parking in the kernel when the count is zero.
	pub fn (mut sem Semaphore) wait() {
		if sem.try_wait() {
			return
		}
		C.atomic_fetch_add_u32(&sem.waiters, 1)
		defer { C.atomic_fetch_sub_u32(&sem.waiters, 1) }
		for !sem.try_wait() {
			wait_word(&sem.count, 0)
		}
	}

	// timed_wait consumes a permit or returns false after timeout nanoseconds.
	// Spurious wakes and signals retain the original monotonic deadline.
	pub fn (mut sem Semaphore) timed_wait(timeout i64) bool {
		if sem.try_wait() {
			return true
		}
		if timeout <= 0 {
			return false
		}
		start := sync_mono_now()
		C.atomic_fetch_add_u32(&sem.waiters, 1)
		defer { C.atomic_fetch_sub_u32(&sem.waiters, 1) }
		for {
			if sem.try_wait() {
				return true
			}
			elapsed := sync_mono_now() - start
			if elapsed >= timeout {
				return false
			}
			code := wait_word_error(&sem.count, 0, timeout - elapsed)
			if code == 110 {
				return false
			}
			if code !in [0, 4, 11] {
				cpanic(code)
			}
		}
	}

	// destroy validates that no thread is waiting. It owns no kernel handle.
	pub fn (mut sem Semaphore) destroy() {
		if C.atomic_load_u32(&sem.waiters) != 0 {
			panic('sync: destroying a semaphore with waiters')
		}
	}
}
