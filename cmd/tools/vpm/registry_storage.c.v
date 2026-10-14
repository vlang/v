module main

import os
import uuid

#include "@VMODROOT/cmd/tools/vpm/registry_storage.h"

fn C.vpm_registry_create_file(path voidptr, stream &voidptr) i32
fn C.vpm_registry_write_file(stream voidptr, data voidptr, length usize) i32
fn C.vpm_registry_close_file(stream voidptr) i32
fn C.vpm_registry_sync_file(stream voidptr) i32
fn C.vpm_registry_replace_file(source voidptr, target voidptr) i32
fn C.vpm_registry_sync_directory(path voidptr) i32

struct RegistryTemporaryFile {
	path string
mut:
	stream voidptr
	opened bool
}

fn registry_create_file(path string) !RegistryTemporaryFile {
	mut stream := voidptr(0)
	mut code := i32(0)
	$if windows {
		wide := path.to_wide()
		defer {
			// to_wide allocates a native buffer owned by this call.
			unsafe { free(voidptr(wide)) }
		}
		code = C.vpm_registry_create_file(wide, &stream)
	} $else {
		code = C.vpm_registry_create_file(path.str, &stream)
	}
	if code != 0 {
		return error('failed to create registry temporary file `${path}` (I/O error ${code})')
	}
	return RegistryTemporaryFile{
		path:   path
		stream: stream
		opened: true
	}
}

fn (mut file RegistryTemporaryFile) write(content string) ! {
	if !file.opened {
		return error('registry temporary file is closed')
	}
	code := C.vpm_registry_write_file(file.stream, content.str, usize(content.len))
	if code != 0 {
		return error('failed to write registry temporary file `${file.path}` (I/O error ${code})')
	}
}

fn (mut file RegistryTemporaryFile) close() ! {
	if !file.opened {
		return
	}
	file.opened = false
	code := C.vpm_registry_close_file(file.stream)
	if code != 0 {
		return error('failed to close registry temporary file `${file.path}` (I/O error ${code})')
	}
}

fn (mut file RegistryTemporaryFile) sync() ! {
	if !file.opened {
		return error('registry temporary file is closed')
	}
	code := C.vpm_registry_sync_file(file.stream)
	if code != 0 {
		return error('failed to synchronize registry temporary file `${file.path}` (I/O error ${code})')
	}
}

fn registry_sync_directory(path string) ! {
	code := C.vpm_registry_sync_directory(path.str)
	if code != 0 {
		return error('failed to synchronize registry directory `${path}` (I/O error ${code})')
	}
}

fn registry_mkdir_all(path string) ! {
	registry_mkdir_all_using(path, registry_sync_directory)!
}

fn registry_mkdir_all_using(path string, sync_directory fn (string) !) ! {
	os.mkdir_all(path)!
	// Revisit existing ancestors too: an earlier attempt may have created them
	// but failed to synchronize their entries. Production paths are under cwd.
	boundary := os.abs_path(os.getwd())
	mut current := os.abs_path(path)
	for {
		sync_directory(current)!
		if current == boundary {
			break
		}
		parent := os.dir(current)
		if parent == current {
			break
		}
		current = parent
	}
}

fn registry_replace_file(source string, target string) ! {
	mut code := i32(0)
	$if windows {
		wide_source := source.to_wide()
		wide_target := target.to_wide()
		defer {
			unsafe {
				free(voidptr(wide_source))
				free(voidptr(wide_target))
			}
		}
		code = C.vpm_registry_replace_file(wide_source, wide_target)
	} $else {
		code = C.vpm_registry_replace_file(source.str, target.str)
	}
	if code != 0 {
		return error('failed to replace registry file `${target}` (I/O error ${code})')
	}
}

fn registry_write_file(mut file RegistryTemporaryFile, content string) ! {
	file.write(content)!
}

fn registry_close_file(mut file RegistryTemporaryFile) ! {
	file.close()!
}

fn registry_sync_file(mut file RegistryTemporaryFile) ! {
	file.sync()!
}

fn registry_store_file(path string, content string) ! {
	registry_store_file_using(path, content, registry_write_file, registry_sync_file,
		registry_close_file, registry_replace_file, registry_sync_directory)!
}

// Private operations keep write/sync/close/rename failures testable without disk exhaustion.
fn registry_store_file_using(path string, content string,
	write_file fn (mut RegistryTemporaryFile, string) !,
	sync_file fn (mut RegistryTemporaryFile) !,
	close_file fn (mut RegistryTemporaryFile) !,
	replace_file fn (string, string) !,
	sync_directory fn (string) !) ! {
	// A short basename also works when the target name is near NAME_MAX.
	temporary := os.join_path(os.dir(path), '.vpm-${uuid.new_v4().str()}.tmp')
	mut file := registry_create_file(temporary)!
	// Cleanup starts only after exclusive creation succeeds, so a collision
	// cannot remove another writer's file.
	defer {
		file.close() or {}
		os.rm(temporary) or {}
	}
	write_file(mut file, content)!
	$if !windows {
		if previous := os.stat(path) {
			if previous.get_filetype() == .regular {
				os.chmod(temporary, int(previous.mode & 0o777))!
			}
		}
	}
	sync_file(mut file)!
	close_file(mut file)!
	replace_file(temporary, path)!
	sync_directory(os.dir(path)) or {
		return error('registry file `${path}` was replaced, but directory synchronization failed; durability is uncertain: ${err.msg()}')
	}
}
