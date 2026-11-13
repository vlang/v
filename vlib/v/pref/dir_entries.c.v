module pref

import os

// dir_has_v_entry reports whether `dir` lists an entry whose name ends in `.v`. It
// stops at the first one, where os.ls lists the whole directory: module lookups
// probe directories that hold thousands of files. It fails when `dir` cannot be read.
fn dir_has_v_entry(dir string) !bool {
	$if windows || freestanding || syscalls {
		entries := os.ls(dir)!
		return entries.any(it.ends_with('.v'))
	} $else {
		// readdir needs the C directory stream that os.ls keeps private.
		dir_ptr := unsafe { C.opendir(&char(dir.str)) }
		if isnil(dir_ptr) {
			return error('cannot list ${dir}')
		}
		defer {
			C.closedir(dir_ptr)
		}
		for {
			ent := C.readdir(dir_ptr)
			if isnil(ent) {
				return false
			}
			// The entry name is a NUL-terminated C array; only its last two bytes matter.
			name := unsafe { &u8(&ent.d_name[0]) }
			length := unsafe { C.strlen(&char(name)) }
			if length >= 2 && unsafe { name[length - 2] } == `.` && unsafe { name[length - 1] } == `v` {
				return true
			}
		}
	}
	return false
}
