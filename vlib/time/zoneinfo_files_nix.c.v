@[has_globals]
module time

#include <pthread.h>

// The time zone data is read with the C library directly, rather than through
// `os`: every program that imports `time` would otherwise have to compile `os`,
// and the modules it imports, too.

fn C.getenv(&char) &char

fn C.readlink(pathname &char, buf &char, bufsiz usize) i32

@[typedef]
struct C.pthread_mutex_t {}

const zoneinfo_vroot_zip = @VEXEROOT + '/vlib/time/tzdata/zoneinfo.zip'

@[cinit]
__global zoneinfo_loaders_mutex C.pthread_mutex_t = C.PTHREAD_MUTEX_INITIALIZER

// zoneinfo_loaders_lock waits for the registered time zone loaders.
fn zoneinfo_loaders_lock() {
	C.pthread_mutex_lock(&zoneinfo_loaders_mutex)
}

// zoneinfo_loaders_unlock releases the registered time zone loaders.
fn zoneinfo_loaders_unlock() {
	C.pthread_mutex_unlock(&zoneinfo_loaders_mutex)
}

// zoneinfo_getenv returns the value of the environment variable `name`, when it is set.
fn zoneinfo_getenv(name string) ?string {
	value := unsafe { C.getenv(&char(name.str)) }
	if value == unsafe { nil } {
		return none
	}
	return unsafe { cstring_to_vstring(value) }
}

// zoneinfo_is_abs_path reports whether `path` starts at the root directory.
fn zoneinfo_is_abs_path(path string) bool {
	return path.len > 0 && path[0] == `/`
}

// zoneinfo_join returns the path of `name` inside the directory `dir`.
fn zoneinfo_join(dir string, name string) string {
	return dir.trim_right('/') + '/' + name
}

// zoneinfo_exists reports whether `path` names a file or a directory.
fn zoneinfo_exists(path string) bool {
	return C.access(&char(path.str), 0) == 0
}

// zoneinfo_is_dir reports whether `path` names a directory. It resolves the `.`
// entry of the directory, which takes the permission to search it and not the one
// to list it: the files of a directory that can only be searched are still read
// by their names.
fn zoneinfo_is_dir(path string) bool {
	own_entry := path + '/.'
	return C.access(&char(own_entry.str), 0) == 0
}

// zoneinfo_is_file reports whether `path` names a file.
fn zoneinfo_is_file(path string) bool {
	return zoneinfo_exists(path) && !zoneinfo_is_dir(path)
}

// zoneinfo_readlink returns the target of the symbolic link `path`, when it is one.
fn zoneinfo_readlink(path string) ?string {
	mut buf := [4096]u8{}
	len := C.readlink(&char(path.str), &char(&buf[0]), usize(buf.len))
	if len <= 0 || len >= buf.len {
		return none
	}
	return unsafe { (&buf[0]).vstring_with_len(len).clone() }
}

// zoneinfo_read_file returns the contents of the file `path`.
fn zoneinfo_read_file(path string) ![]u8 {
	if zoneinfo_is_dir(path) {
		return error('"${path}" is a directory')
	}
	file := C.fopen(&char(path.str), c'rb')
	if file == unsafe { nil } {
		return error('failed to open "${path}"')
	}
	defer {
		C.fclose(file)
	}
	mut data := []u8{}
	mut buf := [4096]u8{}
	for {
		len := int(C.fread(&buf[0], 1, usize(buf.len), file))
		if len <= 0 {
			break
		}
		unsafe { data.push_many(&buf[0], len) }
	}
	return data
}
