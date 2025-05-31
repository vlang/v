module filelock

import time

pub struct FileLock {
	name string
mut:
	fd i64
}

pub fn new(fileName string) FileLock {
	return FileLock{
		name: fileName
		fd:   -1
	}
}

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

pub fn (mut l FileLock) release() bool {
	if l.fd != -1 {
		unsafe {
			l.unlink()
		}
		return true
	}
	return false
}
