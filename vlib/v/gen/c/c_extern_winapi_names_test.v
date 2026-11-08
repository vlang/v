module c

import os

// winapi_names_project writes a program that links C functions of its own which
// share their names with Win32 APIs. The object ships no header, so V writes their
// prototypes itself.
@[unsafe]
fn winapi_names_project(name string) !string {
	root := os.join_path(os.vtmp_dir(), 'c_extern_winapi_names_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	os.write_file(os.join_path(root, 'v.mod'), "Module {\n\tname: 'winapi_names'\n}\n")!
	os.write_file(os.join_path(root, 'winapi_names.c'), 'int Sleep(int ms) { return ms + 1; }
int CloseHandle(int handle) { return handle * 2; }
int CreateFile(int flags) { return flags + 100; }
int GetFileInformationByHandle(void* handle, void* information) { return 42; }
')!
	os.write_file(os.join_path(root, 'main.v'), 'module main

#flag @VMODROOT/winapi_names.o

fn C.Sleep(ms int) int
fn C.CloseHandle(handle int) int
fn C.CreateFile(flags int) int
fn C.GetFileInformationByHandle(handle voidptr, information voidptr) i32

fn main() {
	println(C.Sleep(41))
	println(C.CloseHandle(21))
	println(C.CreateFile(1))
	println(C.GetFileInformationByHandle(unsafe { nil }, unsafe { nil }))
	create_file := voidptr(&C.CreateFile)
	println(create_file != unsafe { nil })
}
')!
	return root
}

fn test_program_owned_winapi_names_keep_their_meaning_off_windows() {
	root := winapi_names_project('generated')!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	for target_os in ['linux', 'macos'] {
		c_source := os.join_path(root, '${target_os}.c')
		result := os.exec([@VEXE, '-new-compiler', '-nocache', '-os', target_os, '-gc', 'none',
			'-o', c_source, source])
		assert result.exit_code == 0, result.output
		generated := os.read_lines(c_source)!
		// These program-owned names have no Win32 calling convention or suffix.
		for line in ['int Sleep(int ms);', 'int CloseHandle(int handle);', 'int CreateFile(int flags);',
			'i32 GetFileInformationByHandle(void* handle, void* information);',
			'\tprintln(int__str(CreateFile(1)));', '\tvoid* create_file = CreateFile;'] {
			assert line in generated, '${target_os}: ${line}'
		}
	}
}

@[unsafe]
fn test_windows_winapi_names_respect_sdk_ownership_and_headerless_prototypes() {
	root := os.join_path(os.vtmp_dir(), 'c_extern_real_winapi_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.c.v')
	// Windows owns these API names. Unlike the Unix custom-symbol fixture above,
	// these declarations and calls match the SDK ABI and can be syntax checked.
	program := 'module main

fn C.Sleep(ms u32)
fn C.CloseHandle(handle voidptr) i32
fn C.CreateFile(name &u16, desired_access u32, share_mode u32, security voidptr,
	creation u32, flags u32, template_file voidptr) voidptr
fn C.GetFileInformationByHandle(handle voidptr, information voidptr) i32

fn main() {
	C.Sleep(1)
	C.CloseHandle(unsafe { nil })
	C.GetFileInformationByHandle(unsafe { nil }, unsafe { nil })
	file := C.CreateFile(unsafe { nil }, 0, 0, unsafe { nil }, 3, 0, unsafe { nil })
	println(file == unsafe { nil })
	file_proc := voidptr(&C.CreateFile)
	println(file_proc != unsafe { nil })
}
'
	for with_headers in [false, true] {
		os.write_file(source, if with_headers {
			'#include <windows.h>\n' + program
		} else {
			program
		})!
		c_source := os.join_path(root, 'windows_${with_headers}.c')
		mut args := [@VEXE, '-new-compiler', '-nocache', '-os', 'windows', '-gc', 'none']
		if !with_headers {
			// The Windows overflow reporter includes the SDK; disable it to exercise
			// the headerless declarations independently of that runtime helper.
			args << ['-d', 'no_segfault_handler']
		}
		args << ['-o', c_source, source]
		result := os.exec(args)
		assert result.exit_code == 0, result.output
		generated := os.read_lines(c_source)!
		for declaration in ['void WINAPI Sleep(u32 ms);', 'i32 WINAPI CloseHandle(void* handle);',
			'void* WINAPI CreateFileW(u16* name, u32 desired_access, u32 share_mode, void* security, ' +
				'u32 creation, u32 flags, void* template_file);',
			'i32 WINAPI GetFileInformationByHandle(void* handle, void* information);'] {
			assert (declaration in generated) == !with_headers, '${with_headers}: ${declaration}'
		}
		assert generated.any(it.starts_with('\tvoid* file = CreateFileW(')), c_source
		assert '\tvoid* file_proc = CreateFileW;' in generated, c_source
		cc_name := $if windows { 'gcc' } $else { 'x86_64-w64-mingw32-gcc' }
		if cc := os.find_abs_path_of_executable(cc_name) {
			checked := os.exec([cc, '-fsyntax-only', c_source])
			assert checked.exit_code == 0, checked.output
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
	// Portable output must also keep these program-owned declarations outside
	// Windows guards, or compiling the generated C loses their prototypes.
	for mode in ['native', 'cross'] {
		executable := os.join_path(root, 'program_${mode}')
		mut args := [@VEXE, '-new-compiler', '-nocache', '-gc', 'none']
		if mode == 'cross' {
			args << '-cross'
		}
		args << ['-o', executable, os.join_path(root, 'main.v')]
		build := os.exec(args)
		assert build.exit_code == 0, '${mode}: ${build.output}'
		run := os.exec([executable])
		assert run.exit_code == 0, '${mode}: ${run.output}'
		assert run.output.split_into_lines() == ['42', '42', '101', '42', 'true'], '${mode}: ${run.output}'
	}
}
