module c

import os
import v.modulecache
import v.pref

const mock_windows_stack_overflow_api = r'
#define WINAPI
#define CALLBACK
#define NULL ((void*)0)
#define EXCEPTION_STACK_OVERFLOW 0xC00000FD
#define EXCEPTION_CONTINUE_SEARCH 0
#define STD_ERROR_HANDLE ((DWORD)-12)
typedef unsigned long ULONG;
typedef unsigned long DWORD;
typedef ULONG* PULONG;
typedef int BOOL;
typedef long LONG;
typedef void (*FARPROC)(void);
typedef struct { DWORD ExceptionCode; } EXCEPTION_RECORD;
typedef struct { EXCEPTION_RECORD* ExceptionRecord; } EXCEPTION_POINTERS;
static int mock_reservations;
static int mock_registrations;
static LONG (*mock_handler)(EXCEPTION_POINTERS*);
static int mock_writes;
static int mock_message_matches;
static int mock_terminations;
static unsigned mock_exit_code;
static inline BOOL mock_stack_guarantee(PULONG size) {
	if (*size == 64 * 1024) ++mock_reservations;
	return 1;
}
static inline void* GetModuleHandleW(const void* name) { return NULL; }
static inline FARPROC GetProcAddress(void* module, const char* name) {
	return (FARPROC)mock_stack_guarantee;
}
static inline void* AddVectoredExceptionHandler(ULONG first, LONG (*handler)(EXCEPTION_POINTERS*)) {
	if (first == 1 && handler != NULL) {
		++mock_registrations;
		mock_handler = handler;
	}
	return NULL;
}
static inline void* GetStdHandle(DWORD handle) { return NULL; }
static inline BOOL WriteFile(void* handle, const void* buffer, DWORD size, DWORD* written, void* overlapped) {
	static const char expected[] = "V panic: stack overflow\n";
	const char* message = (const char*)buffer;
	++mock_writes;
	mock_message_matches = size == sizeof(expected) - 1;
	for (DWORD i = 0; mock_message_matches && i < size; ++i) {
		if (message[i] != expected[i]) mock_message_matches = 0;
	}
	*written = size;
	return 1;
}
static inline void* GetCurrentProcess(void) { return NULL; }
static inline BOOL TerminateProcess(void* process, unsigned code) {
	++mock_terminations;
	mock_exit_code = code;
	return 1;
}
'

const mock_windows_stack_overflow_program = r'
#include "segfault_handler_windows.h"
int main(void) {
	v_install_windows_stack_overflow_handler();
	v_windows_set_stack_guarantee();
	if (mock_reservations != 2 || mock_registrations != EXPECT_REGISTRATIONS) return 1;
	if (EXPECT_REGISTRATIONS == 1) {
		EXCEPTION_RECORD record = {0xE0123456};
		EXCEPTION_POINTERS exception = {&record};
		if (mock_handler(&exception) != EXCEPTION_CONTINUE_SEARCH) return 2;
		if (mock_writes != 0 || mock_terminations != 0) return 3;
		record.ExceptionCode = EXCEPTION_STACK_OVERFLOW;
		mock_handler(&exception);
		if (mock_writes != 1 || !mock_message_matches) return 4;
		if (mock_terminations != 1 || mock_exit_code != 1) return 5;
	} else if (mock_handler != NULL || mock_writes != 0 || mock_terminations != 0) {
		return 6;
	}
	return 0;
}
'

fn test_windows_stack_overflow_installer_leaves_sanitizers_in_control() ! {
	cc_name := $if windows { 'gcc' } $else { 'cc' }
	cc := os.find_abs_path_of_executable(cc_name) or { return }
	dir := os.join_path(os.vtmp_dir(), 'windows_stack_overflow_sanitizers_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'installer.c')
	header_dir := os.join_path(@VEXEROOT, 'vlib', 'builtin')
	os.write_file(os.join_path(dir, 'windows.h'), mock_windows_stack_overflow_api)!
	os.write_file(source, mock_windows_stack_overflow_program)!
	configs := [
		['-DEXPECT_REGISTRATIONS=1'],
		['-DEXPECT_REGISTRATIONS=1', '-DCUSTOM_DEFINE_no_backtrace=1'],
		['-DEXPECT_REGISTRATIONS=0', '-D__SANITIZE_ADDRESS__=1'],
		['-DEXPECT_REGISTRATIONS=0', '-D__SANITIZE_THREAD__=1'],
		['-DEXPECT_REGISTRATIONS=0', '-D__SANITIZE_HWADDRESS__=1'],
		['-DEXPECT_REGISTRATIONS=0', '-D__SANITIZE_ADDRESS__=1', '-DCUSTOM_DEFINE_no_backtrace=1'],
	]
	for i, flags in configs {
		binary := os.join_path(dir, 'installer_${i}.exe')
		compiled := os.exec([cc, '-Wall', '-Werror', '-I', dir, '-I', header_dir, ...flags, source,
			'-o', binary])
		assert compiled.exit_code == 0, compiled.output
		result := os.exec([binary])
		assert result.exit_code == 0, '${flags}: ${result.output}'
	}
	if clang := os.find_abs_path_of_executable('clang') {
		// Preprocess with real sanitizer features, then run the resulting installer
		// without requiring a sanitizer runtime or a native Windows host.
		for sanitizer in ['address', 'thread', 'memory', 'hwaddress'] {
			feature_source := os.join_path(dir, 'installer_${sanitizer}.c')
			preprocessed := os.join_path(dir, 'installer_${sanitizer}.i')
			binary := os.join_path(dir, 'installer_${sanitizer}.exe')
			os.write_file(feature_source, '#if !__has_feature(${sanitizer}_sanitizer)\n#error Expected an active Clang sanitizer feature\n#endif\n' +
				mock_windows_stack_overflow_program)!
			processed := os.exec([clang, '-target', 'aarch64-unknown-linux-gnu', '-E',
				'-fsanitize=${sanitizer}', '-U__SANITIZE_ADDRESS__', '-U__SANITIZE_THREAD__',
				'-U__SANITIZE_HWADDRESS__', '-DEXPECT_REGISTRATIONS=0', '-I', dir, '-I', header_dir,
				feature_source, '-o', preprocessed])
			assert processed.exit_code == 0, processed.output
			compiled := os.exec([cc, '-Wall', '-Werror', preprocessed, '-o', binary])
			assert compiled.exit_code == 0, compiled.output
			result := os.exec([binary])
			assert result.exit_code == 0, '${sanitizer}: ${result.output}'
		}
	}
}

fn test_windows_spawn_wrapper_reserves_stack_before_the_user_call() {
	mut g := FlatGen.new()
	g.set_target(pref.target_from('windows', 'amd64') or { panic(err) })
	g.has_builtins = true
	body := g.spawn_wrapper_body('recurse()', 'void', '')
	assert body.starts_with('v_windows_set_stack_guarantee(); recurse();'), body
	g.compile_defines << 'no_backtrace'
	assert g.spawn_wrapper_body('recurse()', 'void', '').contains('v_windows_set_stack_guarantee();')
	g.compile_defines << 'no_segfault_handler'
	assert !g.spawn_wrapper_body('recurse()', 'void', '').contains('v_windows_set_stack_guarantee();')
}

fn test_windows_stack_overflow_header_is_replicable() ! {
	header := os.read_file(os.join_path(@VEXEROOT, 'vlib', 'builtin',
		'segfault_handler_windows.h'))!
	assert modulecache.c_source_is_replicable(header)
	assert !modulecache.c_source_replicated_function_has_static_storage(header)
}

fn test_windows_stack_overflow_header_and_spawned_program_compile() ! {
	dir := os.join_path(os.vtmp_dir(), 'windows_stack_overflow_codegen_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'main.v')
	c_source := os.join_path(dir, 'main.c')
	os.write_file(source, 'fn recurse(n int) int { if n == 0 { return 0 }; return 1 + recurse(n - 1) }
fn main() { worker := spawn recurse(10000000); println(worker.wait()) }
')!
	for enabled in [true, false] {
		mut flags := [@VEXE, '-new-compiler', '-nocache', '-os', 'windows', '-gc', 'none']
		if !enabled {
			flags << ['-d', 'no_segfault_handler']
		}
		flags << ['-o', c_source, source]
		generated_result := os.exec(flags)
		assert generated_result.exit_code == 0, generated_result.output
		generated := os.read_file(c_source)!
		assert generated.contains('v_windows_set_stack_guarantee();') == enabled
		assert generated.contains('segfault_handler_windows.h') == enabled
		assert generated.contains('v_install_windows_stack_overflow_handler();') == enabled
		cc_name := $if windows { 'gcc' } $else { 'x86_64-w64-mingw32-gcc' }
		if cc := os.find_abs_path_of_executable(cc_name) {
			// The complete runtime has existing MinGW warnings unrelated to exception
			// handling; check the new header separately with warnings as errors.
			header := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'segfault_handler_windows.h')
			checked_header := os.exec([cc, '-Werror', '-Wall', '-fsyntax-only', '-x', 'c', header])
			assert checked_header.exit_code == 0, checked_header.output
			compiled := os.exec([cc, '-fsyntax-only', c_source])
			assert compiled.exit_code == 0, compiled.output
		}
	}
}
