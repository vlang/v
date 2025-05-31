module filelock

fn C.DeleteFileW(&u16) bool
fn C.CreateFileW(&u16, u32, u32, voidptr, u32, u32, voidptr) voidptr
fn C.CloseHandle(voidptr) bool

pub fn (mut l FileLock) unlink() {
	if l.fd != -1 {
		C.CloseHandle(voidptr(l.fd))
		l.fd = -1
	}
	t_wide := l.name.to_wide()
	C.DeleteFileW(t_wide)
}

pub fn (mut l FileLock) acquire() ! {
	if l.fd != -1 {
		return error_with_code('lock already acquired by this instance', 1)
	}
	fd := open(l.name)
	if fd == -1 {
		return error_with_code('cannot create lock file ${l.name}', -1)
	}
	l.fd = fd
}

fn open(f string) i64 {
	f_wide := f.to_wide()
	// locking it
	fd := C.CreateFileW(f_wide, C.GENERIC_READ | C.GENERIC_WRITE, 0, 0, C.OPEN_ALWAYS,
		C.FILE_ATTRIBUTE_NORMAL, 0)
	return i64(fd)
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
