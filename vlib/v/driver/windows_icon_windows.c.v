module driver

import os

#include <windows.h>

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

// windows_manifest_step_for gathers what is beside `exe_path` and returns the manifest
// step for it. Reading the embedded resources is skipped when a sidecar or MSVC decides first.
fn windows_manifest_step_for(exe_path string, is_msvc bool) !WindowsManifestStep {
	sidecar := '${exe_path}.manifest'
	has_sidecar := os.is_file(sidecar)
	generated := has_sidecar && (os.read_file(sidecar) or { '' }) == windows_long_path_manifest
	embedded := if is_msvc || has_sidecar {
		false
	} else {
		windows_executable_has_manifest(exe_path)!
	}
	return windows_manifest_step(is_msvc, has_sidecar, generated, embedded)
}

// add_windows_long_path_manifest writes a sidecar only when neither the linker nor
// the user supplied a manifest. Updating PE resources after linking would strip
// the COFF symbol table and DWARF sections with long names.
fn add_windows_long_path_manifest(exe_path string) !bool {
	if windows_manifest_step_for(exe_path, false)! != .write_sidecar {
		return false
	}
	os.write_file('${exe_path}.manifest', windows_long_path_manifest)!
	return true
}

// ensure_windows_long_path_manifest applies the manifest step for a linked executable.
// `trace` prints the step, as `-v` and `-showcc` do for every other step after the link.
fn ensure_windows_long_path_manifest(exe_path string, is_msvc bool, report_failure bool, trace bool) {
	// An earlier build of the same output may have generated a sidecar, which MSVC output
	// must not keep: its startup dbghelp would hang with a long TMP in a longPathAware
	// process. User manifests stay.
	step := windows_manifest_step_for(exe_path, is_msvc) or {
		if report_failure {
			eprintln('warning: ${exe_path} is not longPathAware: ${err.msg()}')
		}
		return
	}
	match step {
		.write_sidecar {
			os.write_file('${exe_path}.manifest', windows_long_path_manifest) or {
				if report_failure {
					eprintln('warning: ${exe_path} is not longPathAware: ${err.msg()}')
				}
				return
			}
		}
		.remove_generated_sidecar {
			os.rm('${exe_path}.manifest') or {}
		}
		else {}
	}
	if trace {
		message := windows_manifest_step_message(step, exe_path)
		if message != '' {
			println(message)
		}
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
