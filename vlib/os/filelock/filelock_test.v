import os
import os.filelock

fn lockfile_path(name string) string {
	return os.join_path(os.vtmp_dir(), 'filelock_test_${os.getpid()}_${name}')
}

fn test_flock() {
	lockfile := lockfile_path('test.lock')
	os.rm(lockfile) or {}
	mut l := filelock.new(lockfile)
	assert !os.exists(lockfile)
	l.acquire() or { panic(err) }
	assert os.exists(lockfile)
	// do stuff
	l.release()
	assert !os.exists(lockfile)
}

fn test_flock_try() {
	lockfile := lockfile_path('test-try.lock')
	os.rm(lockfile) or {}
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
