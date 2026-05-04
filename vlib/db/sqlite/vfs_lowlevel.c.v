module sqlite

type Sig1 = fn (&C.sqlite3_file, &i64) int // https://github.com/vlang/v/issues/16291

type Sig2 = fn (&Sqlite3_file, &int) int // https://github.com/vlang/v/issues/16291

pub type Sqlite3_file = C.sqlite3_file
pub type Sqlite3_vfs = C.sqlite3_vfs

// https://www.sqlite.org/c3ref/vfs.html
type Fn_sqlite3_syscall_ptr = fn ()

// Keep these callback signatures named so v2 does not need to parse qualified
// type names inside inline `fn (...)` struct fields.
type Sqlite3_file_op_fn = fn (&Sqlite3_file) int

type Sqlite3_file_rw_fn = fn (&Sqlite3_file, voidptr, int, i64) int

type Sqlite3_file_truncate_fn = fn (&Sqlite3_file, i64) int

type Sqlite3_file_flag_fn = fn (&Sqlite3_file, int) int

type Sqlite3_file_control_fn = fn (&Sqlite3_file, int, voidptr) int

type Sqlite3_file_shm_map_fn = fn (&Sqlite3_file, int, int, int, &voidptr) int

type Sqlite3_file_shm_lock_fn = fn (&Sqlite3_file, int, int, int) int

type Sqlite3_file_shm_barrier_fn = fn (&Sqlite3_file)

type Sqlite3_file_fetch_fn = fn (&Sqlite3_file, i64, int, &voidptr) int

type Sqlite3_file_unfetch_fn = fn (&Sqlite3_file, i64, voidptr) int

type Sqlite3_vfs_open_fn = fn (&Sqlite3_vfs, &char, &Sqlite3_file, int, &int) int

type Sqlite3_vfs_delete_fn = fn (&Sqlite3_vfs, &char, int) int

type Sqlite3_vfs_access_fn = fn (&Sqlite3_vfs, &char, int, &int) int

type Sqlite3_vfs_fullpathname_fn = fn (&Sqlite3_vfs, &char, int, &char) int

type Sqlite3_vfs_dlopen_fn = fn (&Sqlite3_vfs, &char) voidptr

type Sqlite3_vfs_dlerror_fn = fn (&Sqlite3_vfs, int, &char)

type Sqlite3_vfs_dlsym_fn = fn (&Sqlite3_vfs, voidptr, &char) voidptr

type Sqlite3_vfs_dlclose_fn = fn (&Sqlite3_vfs, voidptr)

type Sqlite3_vfs_randomness_fn = fn (&Sqlite3_vfs, int, &char) int

type Sqlite3_vfs_sleep_fn = fn (&Sqlite3_vfs, int) int

type Sqlite3_vfs_current_time_fn = fn (&Sqlite3_vfs, &f64) int

type Sqlite3_vfs_get_last_error_fn = fn (&Sqlite3_vfs, int, &char) int

type Sqlite3_vfs_current_time_i64_fn = fn (&Sqlite3_vfs, &i64) int

type Sqlite3_vfs_set_system_call_fn = fn (&Sqlite3_vfs, &char, Fn_sqlite3_syscall_ptr) int

type Sqlite3_vfs_get_system_call_fn = fn (&Sqlite3_vfs, &char) Fn_sqlite3_syscall_ptr

type Sqlite3_vfs_next_system_call_fn = fn (&Sqlite3_vfs, &char) &char

// https://www.sqlite.org/c3ref/file.html
pub struct C.sqlite3_file {
pub mut:
	pMethods &C.sqlite3_io_methods // Methods for an open file
}

// https://www.sqlite.org/c3ref/io_methods.html
@[heap]
pub struct C.sqlite3_io_methods {
mut:
	// version 1 and later fields
	iVersion int

	xClose                 Sqlite3_file_op_fn
	xRead                  Sqlite3_file_rw_fn
	xWrite                 Sqlite3_file_rw_fn
	xTruncate              Sqlite3_file_truncate_fn
	xSync                  Sqlite3_file_flag_fn
	xFileSize              Sig1
	xLock                  Sqlite3_file_flag_fn
	xUnlock                Sqlite3_file_flag_fn
	xCheckReservedLock     Sig2
	xFileControl           Sqlite3_file_control_fn
	xSectorSize            Sqlite3_file_op_fn
	xDeviceCharacteristics Sqlite3_file_op_fn
	// version 2 and later fields
	xShmMap     Sqlite3_file_shm_map_fn
	xShmLock    Sqlite3_file_shm_lock_fn
	xShmBarrier Sqlite3_file_shm_barrier_fn
	xShmUnmap   Sqlite3_file_flag_fn
	// version 3 and later fields
	xFetch   Sqlite3_file_fetch_fn
	xUnfetch Sqlite3_file_unfetch_fn
}

pub type Sqlite3_io_methods = C.sqlite3_io_methods

@[heap]
pub struct C.sqlite3_vfs {
pub mut:
	// version 1 and later fields
	iVersion   int          // Structure version number (currently 3)
	szOsFile   int          // Size of subclassed sqlite3_file
	mxPathname int          // Maximum file pathname length
	pNext      &Sqlite3_vfs // Next registered VFS
	zName      &char        // Name of this virtual file system
	pAppData   voidptr      // Pointer to application-specific data

	xOpen   Sqlite3_vfs_open_fn
	xDelete Sqlite3_vfs_delete_fn

	xAccess       Sqlite3_vfs_access_fn
	xFullPathname Sqlite3_vfs_fullpathname_fn
	xDlOpen       Sqlite3_vfs_dlopen_fn
	xDlError      Sqlite3_vfs_dlerror_fn
	xDlSym        Sqlite3_vfs_dlsym_fn // to fn accepting void and returning
	xDlClose      Sqlite3_vfs_dlclose_fn
	xRandomness   Sqlite3_vfs_randomness_fn
	xSleep        Sqlite3_vfs_sleep_fn
	xCurrentTime  Sqlite3_vfs_current_time_fn
	xGetLastError Sqlite3_vfs_get_last_error_fn
	// version two and later only fields
	xCurrentTimeInt64 Sqlite3_vfs_current_time_i64_fn
	// version three and later only fields
	xSetSystemCall  Sqlite3_vfs_set_system_call_fn
	xGetSystemCall  Sqlite3_vfs_get_system_call_fn
	xNextSystemCall Sqlite3_vfs_next_system_call_fn
}

// https://www.sqlite.org/c3ref/vfs_find.html
fn C.sqlite3_vfs_find(&char) &C.sqlite3_vfs
fn C.sqlite3_vfs_register(&C.sqlite3_vfs, i32) i32
fn C.sqlite3_vfs_unregister(&C.sqlite3_vfs) i32

// get_vfs Requests sqlite to return instance of VFS with given name.
// when such vfs is not known, `none` is returned
pub fn get_vfs(name string) !&Sqlite3_vfs {
	res := C.sqlite3_vfs_find(name.str)
	if res != unsafe { nil } {
		return res
	}
	return error('sqlite3_vfs_find returned 0')
}

// get_default_vfs Asks sqlite for default VFS instance
pub fn get_default_vfs() !&Sqlite3_vfs {
	res := C.sqlite3_vfs_find(unsafe { nil })
	if res != unsafe { nil } {
		return res
	}
	return error('sqlite3_vfs_find(0) returned 0')
}

// register_as_nondefault Asks sqlite to register VFS passed in receiver argument as the known VFS.
// more info about VFS: https://www.sqlite.org/c3ref/vfs.html
// 'not TODOs' to prevent corruption: https://sqlite.org/howtocorrupt.html
// example VFS: https://www.sqlite.org/src/doc/trunk/src/test_demovfs.c
pub fn (mut v Sqlite3_vfs) register_as_nondefault() ! {
	res := C.sqlite3_vfs_register(v, 0)
	if sqlite_ok != res {
		return error('sqlite3_vfs_register returned ${res}')
	}
}

// unregister Requests sqlite to stop using VFS as passed in receiver argument
pub fn (mut v Sqlite3_vfs) unregister() ! {
	res := C.sqlite3_vfs_unregister(v)
	if sqlite_ok != res {
		return error('sqlite3_vfs_unregister returned ${res}')
	}
}

// https://www.sqlite.org/c3ref/open.html
fn C.sqlite3_open_v2(&char, &&C.sqlite3, i32, &char) i32

// https://www.sqlite.org/c3ref/c_open_autoproxy.html
pub enum OpenModeFlag {
	readonly     = 0x00000001
	readwrite    = 0x00000002
	create       = 0x00000004
	uri          = 0x00000040
	memory       = 0x00000080
	nomutex      = 0x00008000
	fullmutex    = 0x00010000
	sharedcache  = 0x00020000
	privatecache = 0x00040000
	exrescode    = 0x02000000
	nofollow     = 0x01000000
}

// connect_full Opens connection to sqlite database. It gives more control than `open`.
// Flags give control over readonly and create decisions. Specific VFS can be chosen.
// Pass '' for vfs_name, if you want to use the default VFS for the current platform,
// (which is equivalent to 'win32' on Windows, and 'unix' on !Windows systems).
pub fn connect_full(path string, mode_flags []OpenModeFlag, vfs_name string) !DB {
	db := &C.sqlite3(unsafe { nil })

	mut flags := 0

	for flag in mode_flags {
		flags = flags | int(flag)
	}

	mut pstr := unsafe { &char(0) }
	if vfs_name != '' {
		pstr = vfs_name.str
	}
	code := C.sqlite3_open_v2(&char(path.str), &db, flags, pstr)
	if code != 0 {
		return &SQLError{
			msg:  unsafe { cstring_to_vstring(&char(C.sqlite3_errstr(code))) }
			code: code
		}
	}
	return DB{
		conn:    db
		is_open: true
	}
}
