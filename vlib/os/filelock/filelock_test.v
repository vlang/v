import os
import os.filelock

fn test_flock() {
	lockfile := 'test.lock'
	mut l := filelock.new(lockfile)
	assert !os.exists(lockfile)
	l.acquire() or { panic(err) }
	assert os.exists(lockfile)
	// do stuff
	l.release()
	assert !os.exists(lockfile)
}

fn test_flock_try() {
	lockfile := 'test-try.lock'
	mut l := filelock.new(lockfile)
	assert l.try_acquire()
	l.release()
	assert !os.exists(lockfile)
	assert l.try_acquire()
	assert os.exists(lockfile)
	l.release()
	assert l.try_acquire()
	l.release()
	assert !os.exists(lockfile)
}
