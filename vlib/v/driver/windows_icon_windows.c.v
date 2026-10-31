module driver

import os

#include <windows.h>

@[unsafe]
fn C.LoadLibraryExW(file_name &u16, file voidptr, flags u32) voidptr
fn C.FindResourceW(module_handle voidptr, name voidptr, type_ voidptr) voidptr
fn C.FreeLibrary(module_handle voidptr) bool
fn C.GetLastError() u32
fn C.GetFullPathNameW(file_name &u16, buffer_length u32, buffer &u16, file_part &&u16) u32

const windows_rt_manifest = 24
// LOAD_LIBRARY_AS_DATAFILE | LOAD_LIBRARY_AS_IMAGE_RESOURCE: map the resources only.
const windows_load_as_resources = u32(0x22)

// windows_long_path_manifest declares the executable longPathAware, so Windows lets it
// use paths past MAX_PATH (the setting applies only to executables that declare it).
const windows_long_path_manifest = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
  <application xmlns="urn:schemas-microsoft-com:asm.v3">
    <windowsSettings>
      <longPathAware xmlns="http://schemas.microsoft.com/SMI/2016/WindowsSettings">true</longPathAware>
    </windowsSettings>
  </application>
</assembly>
'

// windows_executable_has_manifest reports whether the executable at `exe_path` already
// carries an application manifest, with one of the ids 1 to 3 that Windows looks for.
fn windows_executable_has_manifest(exe_path string) !bool {
	// The finalized output may be past MAX_PATH even when its build directory
	// was shortened. LoadLibraryExW needs a fully qualified extended path there.
	// os.abs_path preserves current-drive-rooted paths such as `\out\app.exe`.
	// Resolve the drive before adding the extended prefix.
	path := exe_path.replace('/', '\\').to_wide()
	mut buffer := []u16{len: 256}
	mut absolute_path := ''
	for {
		length := C.GetFullPathNameW(path, u32(buffer.len), buffer.data, unsafe { nil })
		if length == 0 {
			return error('failed to resolve `${exe_path}` (Windows error ${C.GetLastError()})')
		}
		if length < u32(buffer.len) {
			// Windows returned `length` UTF-16 code units, excluding the terminator.
			absolute_path = unsafe { string_from_wide2(buffer.data, int(length)) }
			break
		}
		buffer = []u16{len: int(length)}
	}
	windows_exe_path := if absolute_path.starts_with('\\\\?\\') || absolute_path.starts_with('\\\\.\\') {
		absolute_path
	} else if absolute_path.starts_with('\\\\') {
		'\\\\?\\UNC\\' + absolute_path[2..]
	} else {
		'\\\\?\\' + absolute_path
	}
	module_handle := C.LoadLibraryExW(windows_exe_path.to_wide(), unsafe { nil },
		windows_load_as_resources)
	if isnil(module_handle) {
		return error('failed to read the resources of `${windows_exe_path}` (Windows error ${C.GetLastError()})')
	}
	defer {
		C.FreeLibrary(module_handle)
	}
	for id in 1 .. 4 {
		if !isnil(C.FindResourceW(module_handle, windows_resource_id(id),
			windows_resource_id(windows_rt_manifest))) {
			return true
		}
	}
	return false
}

// add_windows_long_path_manifest writes a sidecar only when neither the linker nor
// the user supplied a manifest. Updating PE resources after linking would strip
// the COFF symbol table and DWARF sections with long names.
fn add_windows_long_path_manifest(exe_path string) !bool {
	if os.is_file('${exe_path}.manifest') || windows_executable_has_manifest(exe_path)! {
		return false
	}
	os.write_file('${exe_path}.manifest', windows_long_path_manifest)!
	return true
}

fn ensure_windows_long_path_manifest(exe_path string, is_msvc bool, report_failure bool) {
	if is_msvc {
		// An earlier build of the same output may have generated a sidecar. Keep
		// user manifests, but do not opt MSVC's dbghelp startup into long paths.
		remove_generated_windows_long_path_manifest(exe_path)
		return
	}
	add_windows_long_path_manifest(exe_path) or {
		if report_failure {
			eprintln('warning: ${exe_path} is not longPathAware: ${err.msg()}')
		}
		false
	}
}

fn remove_generated_windows_long_path_manifest(exe_path string) {
	if contents := os.read_file('${exe_path}.manifest') {
		if contents == windows_long_path_manifest {
			os.rm('${exe_path}.manifest') or {}
		}
	}
}

fn windows_resource_id(id int) voidptr {
	return voidptr(uint(id))
}
