module filelock

import time

#include <sys/file.h>

fn C.unlink(&char) int
fn C.open(&char, int, int) int
fn C.flock(int, int) int

[unsafe]
pub fn (mut l FileLock) unlink() {
	if l.fd != -1 {
		C.close(l.fd)
		l.fd = -1
	}
	C.unlink(&char(l.name.str))
}

pub fn (mut l FileLock) acquire() ! {
	if l.fd != -1 {
		return error_with_code('lock already acquired by this instance', 1)
	}
	fd := open_lockfile(l.name)
	if fd == -1 {
		return error_with_code('cannot create lock file $l.name', -1)
	}
	if C.flock(fd, C.LOCK_EX) == -1 {
		C.close(fd)
		return error_with_code('cannot lock', -2)
	}
	l.fd = fd
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

pub fn (mut l FileLock) wait_acquire(s int) bool {
	fin := time.now().add(s)
	for time.now() < fin {
		if l.try_acquire() {
			return true
		}
		C.usleep(1000)
	}
	return false
}

fn open_lockfile(f string) int {
	mut fd := C.open(&char(f.str), C.O_CREAT, 0o644)
	if fd == -1 {
		// if stat is too old delete lockfile
		fd = C.open(&char(f.str), C.O_RDONLY, 0)
	}
	return fd
}

pub fn (mut l FileLock) try_acquire() bool {
	if l.fd != -1 {
		return true
	}
	fd := open_lockfile('$l.name')
	if fd != -1 {
		err := C.flock(fd, C.LOCK_EX | C.LOCK_NB)
		if err == -1 {
			C.close(fd)
			return false
		}
		l.fd = fd
		return true
	}
	return false
}
