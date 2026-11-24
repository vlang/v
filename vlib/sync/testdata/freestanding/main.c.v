module main

import io
import sync

#include <errno.h>

fn require(ok bool, message string) {
	if !ok {
		eprintln('FAIL: ' + message)
		$if vlibc ? {
			vlibc_sys_exit_group(1)
		} $else {
			exit(1)
		}
	}
}

fn basic_checks() {
	mut word := u32(0)
	C.errno = 123
	before := bf_last_errno()
	require(bf_futex_wait(&word, 1, -1) == 11, 'changed futex value')
	require(bf_futex_wait(&word, 0, 0) == 110, 'zero futex timeout')
	woken, wake_error := bf_futex_wake(&word, 1)
	require(woken == 0 && wake_error == 0, 'empty futex wake')
	_, invalid := bf_futex_wake(&word, 0)
	require(invalid == 22, 'invalid futex wake count')
	require(C.errno == 123 && bf_last_errno() == before, 'futex errno preservation')

	mut mu := sync.Mutex{}
	require(mu.try_lock(), 'zero mutex try lock')
	require(!mu.try_lock(), 'locked mutex try lock')
	mu.unlock()
	mu.lock()
	mu.unlock()
	mu.destroy()
	mu.init()
	require(mu.try_lock(), 'reinitialized mutex')
	mu.unlock()
	mu.destroy()

	mut rw := sync.RwMutex{}
	require(rw.try_rlock(), 'zero rw read lock')
	require(rw.try_rlock(), 'simultaneous read locks')
	require(!rw.try_wlock(), 'writer excludes readers')
	rw.runlock()
	rw.runlock()
	require(rw.try_wlock(), 'zero rw write lock')
	require(!rw.try_rlock() && !rw.try_wlock(), 'exclusive write lock')
	rw.unlock()
	rw.lock()
	rw.unlock()
	rw.rlock()
	rw.runlock()
	rw.destroy()
	rw.init()
	rw.destroy()

	mut sem := sync.Semaphore{}
	require(!sem.try_wait(), 'zero semaphore')
	require(!sem.timed_wait(0), 'empty zero deadline')
	require(!sem.timed_wait(-1), 'empty negative deadline')
	require(!sem.timed_wait(1_000_000), 'empty finite deadline')
	sem.post()
	require(sem.timed_wait(0), 'available permit at zero deadline')
	sem.init(3)
	sem.wait()
	require(sem.try_wait() && sem.timed_wait(-1), 'initial permits')
	require(!sem.try_wait(), 'exact permit count')
	sem.destroy()
	sem.init(0xffffffff)
	require(sem.try_wait(), 'maximum initial count')
	sem.post()
	sem.destroy()
	require(sync.thread_id() == bf_gettid(), 'kernel thread ID')

	mut reader, mut writer := io.pipe()
	mut empty := []u8{}
	require((reader.read(mut empty) or { -1 }) == 0, 'pipe empty read')
	require((writer.write([]u8{}) or { -1 }) == 0, 'pipe empty write')
	writer.close()
	mut byte := [u8(0)]
	mut saw_eof := false
	ended := reader.read(mut byte) or {
		saw_eof = true
		-1
	}
	require(saw_eof && ended == -1, 'pipe EOF')
	reader.close()
}

$if vlibc ? {
	struct Counter {
	mut:
		mutex &sync.Mutex     = sync.new_mutex()
		done  &sync.Semaphore = sync.new_semaphore()
		value int
	}

	fn counter_worker(address u64) {
		mut counter := unsafe { &Counter(voidptr(address)) }
		for _ in 0 .. 10_000 {
			counter.mutex.lock()
			counter.value++
			counter.mutex.unlock()
		}
		counter.done.post()
	}

	struct WakeProbe {
	mut:
		ready   &sync.Semaphore = sync.new_semaphore()
		permits &sync.Semaphore = sync.new_semaphore()
		done    &sync.Semaphore = sync.new_semaphore()
		advance &sync.Semaphore = sync.new_semaphore()
	}

	fn semaphore_worker(address u64) {
		mut probe := unsafe { &WakeProbe(voidptr(address)) }
		for _ in 0 .. 200 {
			probe.ready.post()
			require(probe.permits.timed_wait(5_000_000_000), 'concurrent permit wake')
			probe.done.post()
			require(probe.advance.timed_wait(5_000_000_000), 'semaphore round advance')
		}
	}

	struct RwProbe {
	mut:
		rw               &sync.RwMutex   = sync.new_rwmutex()
		ready            &sync.Semaphore = sync.new_semaphore()
		release          &sync.Semaphore = sync.new_semaphore()
		done             &sync.Semaphore = sync.new_semaphore()
		finished_readers u32
		value            int = 42
	}

	fn reader_worker(address u64) {
		mut probe := unsafe { &RwProbe(voidptr(address)) }
		probe.rw.rlock()
		probe.ready.post()
		require(probe.release.timed_wait(5_000_000_000), 'concurrent readers release')
		require(probe.value == 42, 'readers exclude writer')
		for {
			previous := vlibc_load_u32(&probe.finished_readers)
			if vlibc_cas_u32(&probe.finished_readers, previous, previous + 1) {
				break
			}
		}
		probe.rw.runlock()
		probe.done.post()
	}

	fn writer_worker(address u64) {
		mut probe := unsafe { &RwProbe(voidptr(address)) }
		probe.ready.post()
		probe.rw.lock()
		require(vlibc_load_u32(&probe.finished_readers) == 2, 'writer waits for every reader')
		probe.value = 99
		probe.rw.unlock()
		probe.done.post()
	}

	struct PriorityProbe {
	mut:
		rw              &sync.RwMutex   = sync.new_rwmutex()
		started         &sync.Semaphore = sync.new_semaphore()
		writer_acquired &sync.Semaphore = sync.new_semaphore()
		release_writer  &sync.Semaphore = sync.new_semaphore()
		done            &sync.Semaphore = sync.new_semaphore()
		next_order      u32
		writer_order    u32
		reader_order    u32
	}

	fn next_priority_order(word &u32) u32 {
		for {
			previous := vlibc_load_u32(word)
			if vlibc_cas_u32(word, previous, previous + 1) {
				return previous + 1
			}
		}
		return 0
	}

	fn priority_writer(address u64) {
		mut probe := unsafe { &PriorityProbe(voidptr(address)) }
		probe.started.post()
		probe.rw.lock()
		probe.writer_order = next_priority_order(&probe.next_order)
		probe.writer_acquired.post()
		require(probe.release_writer.timed_wait(5_000_000_000), 'release active writer')
		probe.rw.unlock()
		probe.done.post()
	}

	fn blocked_reader(address u64) {
		mut probe := unsafe { &PriorityProbe(voidptr(address)) }
		require(!probe.rw.try_rlock(), 'reader respects active or queued writer')
		probe.started.post()
		probe.rw.rlock()
		probe.reader_order = next_priority_order(&probe.next_order)
		probe.rw.runlock()
		probe.done.post()
	}

	fn rw_waiter_checks() {
		mut active := PriorityProbe{}
		active.rw.lock()
		mut reader, reader_code := vlibc_thread_spawn(blocked_reader, u64(voidptr(&active)))
		require(reader_code == .enoerror, 'blocked reader creation')
		require(active.started.timed_wait(5_000_000_000), 'blocked reader started')
		require(!active.done.timed_wait(1_000_000), 'reader blocks behind active writer')
		active.rw.unlock()
		require(active.done.timed_wait(5_000_000_000), 'writer wakes blocked reader')
		join(mut reader)
		require(active.reader_order == 1, 'blocked reader acquired after release')
		active.rw.destroy()

		mut priority := PriorityProbe{}
		priority.rw.rlock()
		mut writer, writer_code := vlibc_thread_spawn(priority_writer, u64(voidptr(&priority)))
		require(writer_code == .enoerror, 'priority writer creation')
		require(priority.started.timed_wait(5_000_000_000), 'priority writer started')
		started := vlibc_now_monotonic_ns()
		// Main holds a read lock and only the writer can touch the gate. A
		// failed read try-lock proves the writer is in its registration section
		// or already queued, before the late reader can enter that gate.
		for priority.rw.try_rlock() {
			priority.rw.runlock()
			require(vlibc_now_monotonic_ns() - started < 5_000_000_000, 'writer queue enrollment')
		}
		mut late_reader, late_code := vlibc_thread_spawn(blocked_reader, u64(voidptr(&priority)))
		require(late_code == .enoerror, 'late reader creation')
		require(priority.started.timed_wait(5_000_000_000), 'late reader started')
		priority.rw.runlock()
		require(priority.writer_acquired.timed_wait(5_000_000_000), 'queued writer acquired')
		require(priority.writer_order == 1, 'queued writer precedes late reader')
		require(!priority.done.try_wait(), 'reader remains excluded by active writer')
		priority.release_writer.post()
		for _ in 0 .. 2 {
			require(priority.done.timed_wait(5_000_000_000), 'priority waiters finish')
		}
		join(mut writer)
		join(mut late_reader)
		require(priority.reader_order == 2, 'last writer wakes late reader')
		priority.rw.destroy()
	}

	struct PipeProbe {
	mut:
		writer &io.PipeWriter
		done   &sync.Semaphore = sync.new_semaphore()
	}

	fn pipe_worker(address u64) {
		mut probe := unsafe { &PipeProbe(voidptr(address)) }
		for _ in 0 .. 20 {
			n := probe.writer.write([u8(1), 0, 255, 4]) or { -1 }
			require(n == 4, 'blocking pipe write')
		}
		probe.writer.close()
		probe.done.post()
	}

	fn join(mut handle VlibcThread) {
		code := vlibc_thread_join(mut handle)
		require(code == .enoerror, 'thread join')
	}

	fn concurrent_checks() {
		rw_waiter_checks()
		mut counter := Counter{}
		mut threads := []VlibcThread{}
		for _ in 0 .. 4 {
			t, code := vlibc_thread_spawn(counter_worker, u64(voidptr(&counter)))
			require(code == .enoerror, 'counter thread creation')
			threads << t
		}
		for _ in 0 .. 4 {
			require(counter.done.timed_wait(5_000_000_000), 'mutex contention progress')
		}
		for mut handle in threads {
			join(mut handle)
		}
		require(counter.value == 40_000, 'mutex critical-section visibility')
		counter.mutex.destroy()
		counter.done.destroy()

		mut probe := WakeProbe{}
		threads.clear()
		for _ in 0 .. 4 {
			t, code := vlibc_thread_spawn(semaphore_worker, u64(voidptr(&probe)))
			require(code == .enoerror, 'semaphore thread creation')
			threads << t
		}
		for _ in 0 .. 200 {
			for _ in 0 .. 4 {
				require(probe.ready.timed_wait(5_000_000_000), 'semaphore enrollment')
			}
			for _ in 0 .. 4 { probe.permits.post() }
			for _ in 0 .. 4 {
				require(probe.done.timed_wait(5_000_000_000), 'multiple posts wake multiple waiters')
			}
			for _ in 0 .. 4 {
				probe.advance.post()
			}
		}
		for mut handle in threads { join(mut handle) }
		require(!probe.permits.try_wait(), 'all posted permits consumed')
		probe.ready.destroy()
		probe.permits.destroy()
		probe.done.destroy()
		probe.advance.destroy()

		mut readers := RwProbe{}
		threads.clear()
		for _ in 0 .. 2 {
			t, code := vlibc_thread_spawn(reader_worker, u64(voidptr(&readers)))
			require(code == .enoerror, 'reader thread creation')
			threads << t
		}
		for _ in 0 .. 2 {
			require(readers.ready.timed_wait(5_000_000_000), 'parallel readers acquire')
		}
		require(!readers.rw.try_wlock(), 'write try lock with concurrent readers')
		t, code := vlibc_thread_spawn(writer_worker, u64(voidptr(&readers)))
		require(code == .enoerror, 'writer thread creation')
		threads << t
		require(readers.ready.timed_wait(5_000_000_000), 'writer started')
		readers.release.post()
		readers.release.post()
		for _ in 0 .. 3 {
			require(readers.done.timed_wait(5_000_000_000), 'read/write lock wakeup')
		}
		for mut handle in threads { join(mut handle) }
		require(readers.value == 99, 'writer visibility')
		readers.rw.destroy()
		readers.ready.destroy()
		readers.release.destroy()
		readers.done.destroy()

		mut reader, mut writer := io.pipe()
		mut transfer := PipeProbe{ writer: writer }
		mut worker, pipe_code := vlibc_thread_spawn(pipe_worker, u64(voidptr(&transfer)))
		require(pipe_code == .enoerror, 'pipe thread creation')
		mut bytes := []u8{}
		mut chunk := []u8{len: 3}
		for {
			n := reader.read(mut chunk) or { break }
			bytes << chunk[..n]
		}
		require(bytes.len == 80, 'partial pipe reads')
		for i, value in bytes {
			require(value == [u8(1), 0, 255, 4][i % 4], 'pipe binary data')
		}
		require(transfer.done.timed_wait(5_000_000_000), 'pipe completion')
		join(mut worker)
		reader.close()
		transfer.done.destroy()
	}
}

fn main() {
	basic_checks()
	$if vlibc ? {
		concurrent_checks()
	}
	println('freestanding sync ok')
}
