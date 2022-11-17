module sqlite

type Sig1 = fn (&C.sqlite3_file, &i64) int // https://github.com/vlang/v/issues/16291

type Sig2 = fn (&Sqlite3_file, &int) int // https://github.com/vlang/v/issues/16291

pub type Sqlite3_file = C.sqlite3_file

// https://www.sqlite.org/c3ref/file.html
struct C.sqlite3_file {
pub mut:
	pMethods &C.sqlite3_io_methods // Methods for an open file
}

// https://www.sqlite.org/c3ref/io_methods.html
[heap]
struct C.sqlite3_io_methods {
mut:
	// version 1 and later fields
	iVersion int

	xClose                 fn (&Sqlite3_file) int
	xRead                  fn (&Sqlite3_file, voidptr, int, i64) int
	xWrite                 fn (&Sqlite3_file, voidptr, int, i64) int
	xTruncate              fn (&Sqlite3_file, i64) int
	xSync                  fn (&Sqlite3_file, int) int
	xFileSize              Sig1
	xLock                  fn (&Sqlite3_file, int) int
	xUnlock                fn (&Sqlite3_file, int) int
	xCheckReservedLock     Sig2
	xFileControl           fn (&Sqlite3_file, int, voidptr) int
	xSectorSize            fn (&Sqlite3_file) int
	xDeviceCharacteristics fn (&Sqlite3_file) int
	// version 2 and later fields
	xShmMap     fn (&Sqlite3_file, int, int, int, &voidptr) int
	xShmLock    fn (&Sqlite3_file, int, int, int) int
	xShmBarrier fn (&Sqlite3_file)
	xShmUnmap   fn (&Sqlite3_file, int) int
	// version 3 and later fields
	xFetch   fn (&Sqlite3_file, i64, int, &voidptr) int
	xUnfetch fn (&Sqlite3_file, i64, voidptr) int
}

pub type Sqlite3_io_methods = C.sqlite3_io_methods

// https://www.sqlite.org/c3ref/vfs.html
type Fn_sqlite3_syscall_ptr = fn ()

pub type Sqlite3_vfs = C.sqlite3_vfs

[heap]
struct C.sqlite3_vfs {
pub mut:
	// version 1 and later fields
	iVersion   int // Structure version number (currently 3)
	szOsFile   int // Size of subclassed sqlite3_file
	mxPathname int // Maximum file pathname length
	pNext      &Sqlite3_vfs // Next registered VFS
	zName      &char        // Name of this virtual file system
	pAppData   voidptr      // Pointer to application-specific data

	xOpen   fn (&Sqlite3_vfs, &char, &Sqlite3_file, int, &int) int
	xDelete fn (&Sqlite3_vfs, &char, int) int

	xAccess       fn (&Sqlite3_vfs, &char, int, &int) int
	xFullPathname fn (&Sqlite3_vfs, &char, int, &char) int
	xDlOpen       fn (&Sqlite3_vfs, &char) voidptr
	xDlError      fn (&Sqlite3_vfs, int, &char)
	xDlSym        fn (&Sqlite3_vfs, voidptr, &char) voidptr // to fn accepting void and returning
	xDlClose      fn (&Sqlite3_vfs, voidptr)
	xRandomness   fn (&Sqlite3_vfs, int, &char) int
	xSleep        fn (&Sqlite3_vfs, int) int
	xCurrentTime  fn (&Sqlite3_vfs, &f64) int
	xGetLastError fn (&Sqlite3_vfs, int, &char) int
	// version two and later only fields
	xCurrentTimeInt64 fn (&Sqlite3_vfs, &i64) int
	// version three and later only fields
	xSetSystemCall  fn (&Sqlite3_vfs, &char, Fn_sqlite3_syscall_ptr) int
	xGetSystemCall  fn (&Sqlite3_vfs, &char) Fn_sqlite3_syscall_ptr
	xNextSystemCall fn (&Sqlite3_vfs, &char) &char
}

// https://www.sqlite.org/c3ref/vfs_find.html
fn C.sqlite3_vfs_find(&char) &C.sqlite3_vfs
fn C.sqlite3_vfs_register(&C.sqlite3_vfs, int) int
fn C.sqlite3_vfs_unregister(&C.sqlite3_vfs) int

// get_vfs Requests sqlite to return instance of VFS with given name.
// when such vfs is not known, `none` is returned
pub fn get_vfs(name string) ?&Sqlite3_vfs {
	res := C.sqlite3_vfs_find(name.str)

	unsafe {
		if res == nil {
			return none
		} else {
			return res
		}
	}
}

// get_default_vfs Asks sqlite for default VFS instance
pub fn get_default_vfs() ?&Sqlite3_vfs {
	unsafe {
		res := C.sqlite3_vfs_find(nil)
		if res == nil {
			return none
		} else {
			return res
		}
	}
}

// register_as_nondefault Asks sqlite to register VFS passed in receiver argument as the known VFS.
// more info about VFS: https://www.sqlite.org/c3ref/vfs.html
// 'not TODOs' to prevent corruption: https://sqlite.org/howtocorrupt.html
// example VFS: https://www.sqlite.org/src/doc/trunk/src/test_demovfs.c
pub fn (mut v Sqlite3_vfs) register_as_nondefault() ? {
	res := C.sqlite3_vfs_register(v, 0)

	return if sqlite_ok == res { none } else { error('sqlite3_vfs_register returned ${res}') }
}

// unregister Requests sqlite to stop using VFS as passed in receiver argument
pub fn (mut v Sqlite3_vfs) unregister() ? {
	res := C.sqlite3_vfs_unregister(v)

	return if sqlite_ok == res { none } else { error('sqlite3_vfs_unregister returned ${res}') }
}

// https://www.sqlite.org/c3ref/open.html
fn C.sqlite3_open_v2(&char, &&C.sqlite3, int, &char) int

// https://www.sqlite.org/c3ref/c_open_autoproxy.html
pub enum OpenModeFlag {
	readonly = 0x00000001
	readwrite = 0x00000002
	create = 0x00000004
	uri = 0x00000040
	memory = 0x00000080
	nomutex = 0x00008000
	fullmutex = 0x00010000
	sharedcache = 0x00020000
	privatecache = 0x00040000
	exrescode = 0x02000000
	nofollow = 0x01000000
}

// connect_full Opens connection to sqlite database. It gives more control than `open`.
// Flags give control over readonly and create decisions. Specific VFS can be chosen.
pub fn connect_full(path string, mode_flags []OpenModeFlag, vfs_name string) !DB {
	db := &C.sqlite3(0)

	mut flags := 0

	for flag in mode_flags {
		flags = flags | int(flag)
	}

	code := C.sqlite3_open_v2(&char(path.str), &db, flags, vfs_name.str)
	if code != 0 {
		return &SQLError{
			msg: unsafe { cstring_to_vstring(&char(C.sqlite3_errstr(code))) }
			code: code
		}
	}
	return DB{
		conn: db
		is_open: true
	}
}
