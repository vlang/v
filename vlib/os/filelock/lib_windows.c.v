module filelock

#insert "@VEXEROOT/vlib/os/filelock/filelock_helpers.h"

fn C.DeleteFileW(&u16) bool
fn C.CreateFileW(&u16, u32, u32, voidptr, u32, u32, voidptr) voidptr
fn C.CloseHandle(voidptr) bool
fn C.v_filelock_lock(voidptr, int, int, u64, u64) int
fn C.v_filelock_unlock(voidptr, u64, u64) int

fn (l &FileLock) open_lock() i64 {
	if l.target == .file {
		return open_existing_file(l.name, l.mode)
	}
	return open_lockfile(l.name)
}

fn open_lockfile(path string) i64 {
	path_wide := path.to_wide()
	handle := C.CreateFileW(path_wide, C.GENERIC_READ | C.GENERIC_WRITE, 0, 0, C.OPEN_ALWAYS,
		C.FILE_ATTRIBUTE_NORMAL, 0)
	return file_handle(handle)
}

fn open_existing_file(path string, mode LockMode) i64 {
	path_wide := path.to_wide()
	access := if mode == .shared { C.GENERIC_READ } else { C.GENERIC_READ | C.GENERIC_WRITE }
	share_mode := C.FILE_SHARE_READ | C.FILE_SHARE_WRITE | C.FILE_SHARE_DELETE
	handle := C.CreateFileW(path_wide, access, share_mode, 0, C.OPEN_EXISTING,
		C.FILE_ATTRIBUTE_NORMAL, 0)
	return file_handle(handle)
}

fn file_handle(handle voidptr) i64 {
	return if handle == voidptr(-1) { -1 } else { i64(handle) }
}

fn (l &FileLock) lock_handle(handle voidptr, immediate bool) bool {
	if l.target != .file {
		return true
	}
	return C.v_filelock_lock(handle, int(l.mode == .exclusive), int(immediate), l.start, l.len) == 0
}

fn (mut l FileLock) close_lock() {
	if l.fd == -1 {
		return
	}
	handle := voidptr(l.fd)
	if l.target == .file {
		C.v_filelock_unlock(handle, l.start, l.len)
		C.CloseHandle(handle)
	} else {
		C.CloseHandle(handle)
		path_wide := l.name.to_wide()
		C.DeleteFileW(path_wide)
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
	handle := voidptr(fd)
	if !l.lock_handle(handle, false) {
		C.CloseHandle(handle)
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
	handle := voidptr(fd)
	if !l.lock_handle(handle, true) {
		C.CloseHandle(handle)
		return false
	}
	l.fd = fd
	return true
}
