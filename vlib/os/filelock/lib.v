module filelock

import time

// LockMode describes whether an existing file lock is shared/read or exclusive/write.
pub enum LockMode {
	shared
	exclusive
}

// LockOptions configures how `new_file` locks an existing file.
@[params]
pub struct LockOptions {
pub:
	mode  LockMode = .exclusive
	start u64
	len   u64
}

enum LockTarget {
	sidecar
	file
}

pub struct FileLock {
	name   string
	mode   LockMode = .exclusive
	start  u64
	len    u64
	target LockTarget = .sidecar
mut:
	fd i64
}

// new creates a sidecar lock file that is removed again when the lock is released.
pub fn new(file_name string) FileLock {
	return FileLock{
		name: file_name
		fd:   -1
	}
}

// new_file creates a lock for an existing file path using OS-level file locking.
pub fn new_file(path string, options LockOptions) FileLock {
	return FileLock{
		name:   path
		mode:   options.mode
		start:  options.start
		len:    options.len
		target: .file
		fd:     -1
	}
}

// wait_acquire keeps trying to acquire the lock until `timeout` expires.
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

// release unlocks the file and closes the underlying file descriptor or handle.
pub fn (mut l FileLock) release() bool {
	if l.fd != -1 {
		l.close_lock()
		return true
	}
	return false
}
