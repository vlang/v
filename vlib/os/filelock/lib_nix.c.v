module filelock

#include <sys/file.h>
#insert "@VEXEROOT/vlib/os/filelock/filelock_helpers.h"

fn C.unlink(&char) i32
fn C.open(&char, i32, i32) i32
fn C.close(i32) i32
fn C.flock(i32, i32) i32
fn C.v_filelock_lock(i32, i32, i32, u64, u64) i32
fn C.v_filelock_unlock(i32, u64, u64) i32

fn (l &FileLock) open_lock() int {
	if l.target == .file {
		return open_existing_file(l.name, l.mode)
	}
	return open_lockfile(l.name)
}

fn open_lockfile(path string) int {
	mut fd := C.open(&char(path.str), C.O_CREAT | C.O_RDONLY, 0o644)
	if fd == -1 {
		fd = C.open(&char(path.str), C.O_RDONLY, 0)
	}
	return fd
}

fn open_existing_file(path string, mode LockMode) int {
	flags := if mode == .shared { C.O_RDONLY } else { C.O_RDWR }
	return C.open(&char(path.str), flags, 0)
}

fn (l &FileLock) lock_fd(fd int, immediate bool) bool {
	if l.target == .file {
		return C.v_filelock_lock(fd, int(l.mode == .exclusive), int(immediate), l.start,
			l.len) == 0
	}
	flags := if immediate { C.LOCK_EX | C.LOCK_NB } else { C.LOCK_EX }
	return C.flock(fd, flags) == 0
}

fn (mut l FileLock) close_lock() {
	if l.fd == -1 {
		return
	}
	fd := int(l.fd)
	if l.target == .file {
		C.v_filelock_unlock(fd, l.start, l.len)
		C.close(fd)
	} else {
		C.close(fd)
		C.unlink(&char(l.name.str))
	}
	l.fd = -1
}

// acquire blocks until the lock is acquired.
pub fn (mut l FileLock) acquire() ! {
	if l.fd != -1 {
		return error_with_code('lock already acquired by this instance', 1)
	}
	fd := l.open_lock()
	if fd == -1 {
		msg := if l.target == .file {
			'cannot open file ${l.name}'
		} else {
			'cannot create lock file ${l.name}'
		}
		return error_with_code(msg, -1)
	}
	if !l.lock_fd(fd, false) {
		C.close(fd)
		return error_with_code('cannot lock ${l.name}', -2)
	}
	l.fd = fd
}

// try_acquire tries to acquire the lock without blocking.
pub fn (mut l FileLock) try_acquire() bool {
	if l.fd != -1 {
		return true
	}
	fd := l.open_lock()
	if fd == -1 {
		return false
	}
	if !l.lock_fd(fd, true) {
		C.close(fd)
		return false
	}
	l.fd = fd
	return true
}
