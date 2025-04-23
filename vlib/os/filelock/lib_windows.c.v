module filelock

fn C.DeleteFileW(&u16) bool
fn C.CreateFileW(&u16, u32, u32, voidptr, u32, u32, voidptr) voidptr
fn C.CloseHandle(voidptr) bool

// unlink unlink the lock file and release it.
pub fn (mut l FileLock) unlink() {
	if !isnil(l.cfile) {
		C.CloseHandle(l.cfile)
		l.cfile = unsafe { nil }
	}
	t_wide := l.name.to_wide()
	C.DeleteFileW(t_wide)
}

// acquire acquire the lock file.
pub fn (mut l FileLock) acquire() ! {
	if !isnil(l.cfile) {
		return error_with_code('lock already acquired by this instance', 1)
	}
	cfile := open(l.name)
	if isnil(cfile) {
		return error_with_code('cannot create lock file ${l.name}', -1)
	}
	l.cfile = cfile
}

fn open(f string) voidptr {
	f_wide := f.to_wide()
	// locking it
	cfile := C.CreateFileW(f_wide, C.GENERIC_READ | C.GENERIC_WRITE, 0, 0, C.OPEN_ALWAYS,
		C.FILE_ATTRIBUTE_NORMAL, 0)
	return cfile
}

// try_acquire try acquire the lock file, it will return true if success.
pub fn (mut l FileLock) try_acquire() bool {
	if !isnil(l.cfile) {
		// lock already acquired by this instance
		return false
	}
	cfile := open(l.name)
	if isnil(cfile) {
		return false
	}
	l.cfile = cfile
	return true
}

// release release the lock file and unlink it.
pub fn (mut l FileLock) release() bool {
	if !isnil(l.cfile) {
		unsafe {
			l.unlink()
		}
		return true
	}
	return false
}
