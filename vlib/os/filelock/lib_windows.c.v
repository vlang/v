module filelock

import time

fn C.DeleteFileW(&u16) bool
fn C.CreateFileW(&u16, u32, u32, voidptr, u32, u32, voidptr) voidptr
fn C.CloseHandle(voidptr) bool

pub fn (mut l FileLock) unlink() {
	if l.fd != -1 {
		C.CloseHandle(l.fd)
		l.fd = -1
	}
	t_wide := l.name.to_wide()
	C.DeleteFileW(t_wide)
}

pub fn (mut l FileLock) acquire() ?bool {
	if l.fd != -1 {
		// lock already acquired by this instance
		return false
	}
	fd := open(l.name)
	if fd == -1 {
		return error('cannot create lock file $l.name')
	}
	l.fd = fd
	return true
}

pub fn (mut l FileLock) release() bool {
	if l.fd != -1 {
		C.CloseHandle(l.fd)
		l.fd = -1
		t_wide := l.name.to_wide()
		C.DeleteFileW(t_wide)
		return true
	}
	return false
}

pub fn (mut l FileLock) wait_acquire(s int) ?bool {
	fin := time.now().add(s)
	for time.now() < fin {
		if l.try_acquire() {
			return true
		}
		time.sleep(1 * time.millisecond)
	}
	return false
}

fn open(f string) voidptr {
	f_wide := f.to_wide()
	// locking it
	fd := C.CreateFileW(f_wide, C.GENERIC_READ | C.GENERIC_WRITE, 0, 0, C.OPEN_ALWAYS,
		C.FILE_ATTRIBUTE_NORMAL, 0)
	if fd == C.INVALID_HANDLE_VALUE {
		fd == -1
	}
	return fd
}

pub fn (mut l FileLock) try_acquire() bool {
	if l.fd != -1 {
		// lock already acquired by this instance
		return false
	}
	fd := open(l.name)
	if fd == -1 {
		return false
	}
	l.fd = fd
	return true
}
