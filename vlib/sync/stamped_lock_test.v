module sync

import sync.stdatomic { new_atomic }
import time

// An optimistic stamp validates while nobody writes, and fails after a write.
fn test_stamped_lock_optimistic_validate() {
	mut l := new_stamped_lock()
	stamp := l.try_optimistic_read()
	assert l.validate(stamp) == true, 'uncontended stamp must validate'
	l.write_lock()
	l.write_unlock()
	assert l.validate(stamp) == false, 'stamp must fail after a write'
}

// Two readers may hold the lock at the same time.
fn test_stamped_lock_shared_readers() {
	mut l := new_stamped_lock()
	in_ch := chan bool{cap: 2}
	release_ch := chan bool{cap: 2}
	defer {
		in_ch.close()
		release_ch.close()
	}
	for _ in 0 .. 2 {
		spawn fn [mut l, in_ch, release_ch] () {
			l.read_lock()
			in_ch <- true
			_ := <-release_ch
			l.read_unlock()
		}()
	}
	_ := <-in_ch
	_ := <-in_ch // both readers inside at once proves sharing
	release_ch <- true
	release_ch <- true
	assert true, 'shared readers must both proceed'
}

// A writer waits while a reader holds, then proceeds after release.
fn test_stamped_lock_writer_waits_for_reader() ! {
	mut l := new_stamped_lock()
	mut wrote := new_atomic(false)
	done_ch := chan bool{cap: 1}
	l.read_lock()
	writer := spawn fn [mut l, mut wrote, done_ch] () {
		l.write_lock()
		wrote.store(true)
		l.write_unlock()
		done_ch <- true
	}()
	sw := time.new_stopwatch()
	mut queued := false
	for !wrote.load() && sw.elapsed() < 2 * time.second {
		l.cond.inner_mutex.lock()
		queued = l.cond.waiters.len > 0
		l.cond.inner_mutex.unlock()
		if queued {
			break
		}
		time.sleep(time.millisecond)
	}
	blocked := !wrote.load()
	l.read_unlock()
	select {
		_ := <-done_ch {
		}
		2 * time.second {
			return error('writer did not proceed after the reader released the lock')
		}
	}
	writer.wait()
	assert queued && blocked, 'writer must wait while a reader holds the lock'
	assert wrote.load() == true, 'writer must proceed after release'
}

// A stamp taken before a write fails even if the write already finished.
fn test_stamped_lock_version_bumps_on_write() {
	mut l := new_stamped_lock()
	first := l.try_optimistic_read()
	l.write_lock()
	l.write_unlock()
	second := l.try_optimistic_read()
	assert first != second, 'version must advance per write'
	assert l.validate(second) == true
}

fn test_stamped_lock_stamp_during_write_stays_invalid() {
	mut l := new_stamped_lock()
	assert !l.validate(0)
	before := l.try_optimistic_read()
	assert before != 0
	l.write_lock()
	during := l.try_optimistic_read()
	assert during == 0
	assert !l.validate(before)
	assert !l.validate(during)
	l.write_unlock()
	assert !l.validate(during)
	assert !l.validate(before)
	assert l.validate(l.try_optimistic_read())
}
