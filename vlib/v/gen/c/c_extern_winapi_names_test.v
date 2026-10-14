module c

import os

// winapi_names_project writes a program that links C functions of its own which
// share their names with Win32 APIs. The object ships no header, so V writes their
// prototypes itself.
fn winapi_names_project(name string) !string {
	root := os.join_path(os.vtmp_dir(), 'c_extern_winapi_names_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	os.write_file(os.join_path(root, 'v.mod'), "Module {\n\tname: 'winapi_names'\n}\n")!
	os.write_file(os.join_path(root, 'winapi_names.c'), 'int Sleep(int ms) { return ms + 1; }
int CloseHandle(int handle) { return handle * 2; }
int CreateFile(int flags) { return flags + 100; }
')!
	os.write_file(os.join_path(root, 'main.v'), 'module main

#flag @VMODROOT/winapi_names.o

fn C.Sleep(ms int) int
fn C.CloseHandle(handle int) int
fn C.CreateFile(flags int) int

fn main() {
	println(C.Sleep(41))
	println(C.CloseHandle(21))
	println(C.CreateFile(1))
	create_file := voidptr(&C.CreateFile)
	println(create_file != unsafe { nil })
}
')!
	return root
}

fn test_win32_api_names_keep_their_win32_meaning_only_for_windows_targets() {
	root := winapi_names_project('generated')!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	for target_os in ['linux', 'macos', 'windows'] {
		c_source := os.join_path(root, '${target_os}.c')
		result := os.exec([@VEXE, '-new-compiler', '-nocache', '-os', target_os, '-gc', 'none',
			'-o', c_source, source])
		assert result.exit_code == 0, result.output
		generated := os.read_lines(c_source)!
		// `WINAPI` is not defined off Windows, and `CreateFile` is the unsuffixed
		// spelling of the `CreateFileW` export only there.
		call_conv := if target_os == 'windows' { 'WINAPI ' } else { '' }
		create_file := if target_os == 'windows' { 'CreateFileW' } else { 'CreateFile' }
		for line in ['int ${call_conv}Sleep(int ms);', 'int ${call_conv}CloseHandle(int handle);',
			'int ${call_conv}${create_file}(int flags);', '\tprintln(int__str(${create_file}(1)));',
			'\tvoid* create_file = ${create_file};'] {
			assert line in generated, '${target_os}: ${line}'
		}
	}
}

fn test_program_links_its_own_c_functions_named_like_win32_apis() {
	$if windows {
		// <windows.h> is part of every Windows translation unit and owns these names.
		return
	}
	root := winapi_names_project('linked')!
	defer { os.rmdir_all(root) or {} }
	executable := os.join_path(root, 'program')
	build := os.exec([@VEXE, '-new-compiler', '-nocache', '-gc', 'none', '-o', executable,
		os.join_path(root, 'main.v')])
	assert build.exit_code == 0, build.output
	run := os.exec([executable])
	assert run.exit_code == 0, run.output
	assert run.output.split_into_lines() == ['42', '42', '101', 'true'], run.output
}
