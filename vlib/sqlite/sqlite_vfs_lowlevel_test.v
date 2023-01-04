import sqlite
import rand

const (
	max_file_name_len = 256
)

fn test_vfs_register() {
	org_default_vfs := sqlite.get_default_vfs()?

	assert org_default_vfs.zName != 0

	vfs_name := 'sometest'
	mut vfs_descr := &sqlite.Sqlite3_vfs{
		zName: vfs_name.str
		iVersion: 2
	}

	if _ := sqlite.get_vfs(vfs_name) {
		panic('expected that vfs is not known')
	}

	vfs_descr.register_as_nondefault() or { panic('vfs register failed ${err}') }

	sqlite.get_vfs(vfs_name)?

	now_default_vfs := sqlite.get_default_vfs()?

	assert now_default_vfs.zName == org_default_vfs.zName

	vfs_descr.unregister() or { panic('vfs unregister failed ${err}') }

	if _ := sqlite.get_vfs(vfs_name) {
		panic('vfs supposedly unregistered yet somehow still foundable')
	}
}

// minimal vfs based on example https://www.sqlite.org/src/doc/trunk/src/test_demovfs.c
fn test_verify_vfs_is_actually_used() {
	wrapped := sqlite.get_default_vfs()?

	vfs_name := 'sometest'
	mut vfs_state := &ExampleVfsState{
		log: []string{cap: 100}
	}
	mut vfs_descr := &sqlite.Sqlite3_vfs{
		iVersion: 2
		szOsFile: int(sizeof(ExampleVfsOpenedFile))
		mxPathname: max_file_name_len
		zName: vfs_name.str
		pAppData: vfs_state
		xOpen: example_vfs_open
		xDelete: example_vfs_delete
		xAccess: example_vfs_access
		xFullPathname: example_vfs_fullpathname
		xDlOpen: wrapped.xDlOpen
		xDlError: wrapped.xDlError
		xDlSym: wrapped.xDlSym
		xDlClose: wrapped.xDlClose
		xRandomness: wrapped.xRandomness
		xSleep: wrapped.xSleep
		xCurrentTime: wrapped.xCurrentTime
		xGetLastError: example_vfs_getlasterror
		xCurrentTimeInt64: wrapped.xCurrentTimeInt64
	}

	vfs_descr.register_as_nondefault()?

	// normally this would be written to disk
	mut db := sqlite.connect_full('foo.db', [.readwrite, .create], vfs_name)!
	assert ['fullpathname from=foo.db to=foo.db}', 'open temp?=false name=foo.db', 'read file=foo.db'] == vfs_state.log
	vfs_state.log.clear()

	db.close()!
	assert ['close file=foo.db'] == vfs_state.log
}

struct ExampleVfsState {
mut:
	log []string
}

struct ExampleVfsOpenedFile {
mut:
	base      sqlite.Sqlite3_file
	name      string
	vfs_state &ExampleVfsState
}

fn to_vfsstate(t &sqlite.Sqlite3_vfs) &ExampleVfsState {
	unsafe {
		p := t.pAppData
		if p == 0 {
			assert false, 'p should not be 0'
		}
		return &ExampleVfsState(p)
	}
}

fn to_vfsopenedfile(t &sqlite.Sqlite3_file) &ExampleVfsOpenedFile {
	unsafe {
		if t == 0 {
			assert false, 't should not be 0'
		}
		return &ExampleVfsOpenedFile(t)
	}
}

fn example_vfs_fullpathname(vfs &sqlite.Sqlite3_vfs, input &char, size_of_output int, output &char) int {
	println('fullpathname called')

	mut vfs_state := to_vfsstate(vfs)

	from := unsafe { cstring_to_vstring(input) }

	unsafe {
		vmemcpy(output, input, from.len)
		output[from.len] = u8(0)
	}
	result := unsafe { cstring_to_vstring(output) }

	vfs_state.log << 'fullpathname from=${from} to=${result}}'

	return sqlite.sqlite_ok
}

fn example_vfs_access(vfs &sqlite.Sqlite3_vfs, zPath &char, flags int, pResOut &int) int {
	println('access called')
	mut vfs_state := &ExampleVfsState{}

	unsafe {
		assert 0 != vfs.pAppData
		vfs_state = &ExampleVfsState(vfs.pAppData)
	}
	vfs_state.log << 'accessed'

	return sqlite.sqlite_ok
}

fn example_vfs_open(vfs &sqlite.Sqlite3_vfs, file_name_or_null_for_tempfile &char, vfs_opened_file &sqlite.Sqlite3_file, in_flags int, out_flags &int) int {
	println('open called')

	mut is_temp := false
	mut file_name := ''

	unsafe {
		if file_name_or_null_for_tempfile == nil {
			is_temp = true
			file_name = rand.uuid_v4()
		} else {
			file_name = cstring_to_vstring(file_name_or_null_for_tempfile)
		}
	}
	mut vfs_state := to_vfsstate(vfs)

	unsafe {
		mut outp := to_vfsopenedfile(vfs_opened_file)
		outp.base.pMethods = &sqlite.Sqlite3_io_methods{
			iVersion: 1
			xClose: example_vfsfile_close
			xRead: example_vfsfile_read
			xWrite: example_vfsfile_write
			xTruncate: example_vfsfile_truncate
			xSync: example_vfsfile_sync
			xFileSize: example_vfsfile_size
			xLock: example_vfsfile_lock
			xUnlock: example_vfsfile_unlock
			xCheckReservedLock: example_vfsfile_checkreservedlock
			xFileControl: example_vfsfile_filecontrol
			xSectorSize: example_vfsfile_sectorsize
			xDeviceCharacteristics: example_vfsfile_devicecharacteristics
		}

		outp.name = file_name.clone()
		outp.vfs_state = vfs_state
	}
	vfs_state.log << 'open temp?=${is_temp} name=${file_name}'

	return sqlite.sqlite_ok
}

fn example_vfsfile_checkreservedlock(file &sqlite.Sqlite3_file, pResOut &int) int {
	println('file checkreservedlock')

	unsafe {
		*pResOut = 0
	}
	return sqlite.sqlite_ok
}

fn example_vfsfile_filecontrol(file &sqlite.Sqlite3_file, op int, arg voidptr) int {
	println('file filecontrol')

	return 0
}

fn example_vfsfile_devicecharacteristics(file &sqlite.Sqlite3_file) int {
	println('file devicecharacteristics')

	return 0
}

fn example_vfsfile_size(file &sqlite.Sqlite3_file, result &i64) int {
	println('file size')

	return sqlite.sqlite_ok
}

fn example_vfsfile_read(file &sqlite.Sqlite3_file, output voidptr, amount int, offset i64) int {
	println('file read')

	assert amount > 0

	mut vfsfile := to_vfsopenedfile(file)

	vfsfile.vfs_state.log << 'read file=${vfsfile.name}'

	unsafe {
		C.memset(output, 0, amount)
	}

	return sqlite.sqlite_ioerr_short_read
}

fn example_vfsfile_truncate(file &sqlite.Sqlite3_file, size i64) int {
	println('file truncate')

	return sqlite.sqlite_ok
}

fn example_vfsfile_sectorsize(file &sqlite.Sqlite3_file) int {
	println('file sectorsize')

	return 0
}

fn example_vfsfile_sync(file &sqlite.Sqlite3_file, flags int) int {
	println('file sync called')

	return sqlite.sqlite_ok
}

fn example_vfsfile_lock(file &sqlite.Sqlite3_file, elock int) int {
	println('file lock called')

	return sqlite.sqlite_ok
}

fn example_vfsfile_unlock(file &sqlite.Sqlite3_file, elock int) int {
	println('file unlock called')

	return sqlite.sqlite_ok
}

fn example_vfsfile_write(file &sqlite.Sqlite3_file, buf voidptr, amount int, offset i64) int {
	println('file write called')

	return sqlite.sqlite_ok
}

fn example_vfsfile_close(file &sqlite.Sqlite3_file) int {
	println('file close called')

	mut vfsfile := to_vfsopenedfile(file)

	vfsfile.vfs_state.log << 'close file=${vfsfile.name}'

	return sqlite.sqlite_ok
}

fn example_vfs_delete(vfs &sqlite.Sqlite3_vfs, name &char, sync_dir int) int {
	println('vfs delete called')

	return sqlite.sqlite_ok
}

fn example_vfs_getlasterror(vfs &sqlite.Sqlite3_vfs, i int, o &char) int {
	println('vfs getlasterror called')

	unsafe {
		*o = 0
	}
	return sqlite.sqlite_ok
}
