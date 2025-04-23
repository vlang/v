module filelock

import time

pub struct FileLock {
	name string
mut:
	cfile voidptr // Using void* instead of FILE*
	fd    int
}

// new create a new lock file object, it will not create the lock file until `acquire` it.
pub fn new(fileName string) FileLock {
	return FileLock{
		name:  fileName
		fd:    -1
		cfile: unsafe { nil }
	}
}

// wait_acquire try acquire the lock file within a `timeout`.
pub fn (mut l FileLock) wait_acquire(timeout time.Duration) bool {
	fin := time.now().add(timeout)
	for time.now() < fin {
		if l.try_acquire() {
			return true
		}
		time.sleep(1 * time.millisecond)
	}
	return false
}
