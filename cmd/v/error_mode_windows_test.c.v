module main

import os

fn test_windows_loader_dialog_mode_preserves_existing_flags() {
	previous := C.GetErrorMode()
	defer { C.SetErrorMode(previous) }
	C.SetErrorMode((previous | u32(C.SEM_NOOPENFILEERRORBOX)) & ~u32(C.SEM_FAILCRITICALERRORS))
	existing := C.GetErrorMode()
	disable_windows_loader_dialogs()
	assert C.GetErrorMode() == existing | u32(C.SEM_FAILCRITICALERRORS)
}

fn test_doctor_children_inherit_loader_dialog_mode() {
	root := os.join_path(os.vtmp_dir(), 'doctor_error_mode_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'probe.c.v')
	probe := os.join_path(root, 'emcc.exe')
	marker := os.join_path(root, 'inherited-mode')
	os.write_file(source, '
import os
#include <windows.h>
fn C.GetErrorMode() u32
fn main() {
	assert C.GetErrorMode() & u32(C.SEM_FAILCRITICALERRORS) != 0
	os.write_file(os.getenv("V_DOCTOR_ERROR_MODE_MARKER"), "inherited")!
	println("doctor error mode probe")
	exit(37)
}
')!
	build := os.exec([@VEXE, '-o', probe, source])
	assert build.exit_code == 0, build.output
	previous_mode := C.GetErrorMode()
	previous_path := os.getenv('PATH')
	previous_marker := os.getenv_opt('V_DOCTOR_ERROR_MODE_MARKER')
	defer {
		C.SetErrorMode(previous_mode)
		os.setenv('PATH', previous_path, true)
		if value := previous_marker {
			os.setenv('V_DOCTOR_ERROR_MODE_MARKER', value, true)
		} else {
			os.unsetenv('V_DOCTOR_ERROR_MODE_MARKER')
		}
	}
	os.setenv('PATH', root + os.path_delimiter + previous_path, true)
	os.setenv('V_DOCTOR_ERROR_MODE_MARKER', marker, true)
	// Start V without the flag, so only its startup can enable inheritance.
	C.SetErrorMode(previous_mode & ~u32(C.SEM_FAILCRITICALERRORS))
	result := os.exec([@VEXE, 'doctor', '-skip-github'])
	assert result.exit_code == 0, result.output
	assert os.read_file(marker)! == 'inherited'
	assert result.output.contains('Error: doctor error mode probe'), result.output
	assert C.GetErrorMode() == previous_mode & ~u32(C.SEM_FAILCRITICALERRORS)
	// Suppressing dialogs must leave the tool's failure status available to callers.
	disable_windows_loader_dialogs()
	failed := os.exec([probe])
	assert failed.exit_code == 37, failed.output
}
