import os

// Leave these native helper definitions to the C compiler.
const windows_overflow_header = r'
#include <windows.h>

static inline LONG CALLBACK v_test_exception_handler(EXCEPTION_POINTERS* exception) {
	return exception->ExceptionRecord->ExceptionCode == 0xE0123456
		? EXCEPTION_CONTINUE_EXECUTION : EXCEPTION_CONTINUE_SEARCH;
}

static inline void v_test_add_exception_handler(void) {
	// This child exits immediately afterwards; TCC imports omit the remove API.
	AddVectoredExceptionHandler(1, v_test_exception_handler);
}

static inline void v_test_first_chance_exception(void) {
	RaiseException(0xE0123456, 0, 0, NULL);
}

static inline int v_test_stack_guarantee(void) {
	typedef BOOL (WINAPI *stack_guarantee_fn)(PULONG);
	stack_guarantee_fn guarantee = (stack_guarantee_fn)GetProcAddress(
		GetModuleHandleW(L"kernel32.dll"), "SetThreadStackGuarantee");
	ULONG size = 0;
	return guarantee && guarantee(&size) && size >= 64 * 1024;
}

static inline void v_test_disable_error_dialog(void) {
	SetErrorMode(SEM_FAILCRITICALERRORS | SEM_NOGPFAULTERRORBOX);
}
'

const windows_overflow_child_source = r'
module main

import os

#include <@DIR/windows_overflow.h>

fn C.v_test_add_exception_handler()
fn C.v_test_first_chance_exception()
fn C.v_install_windows_stack_overflow_handler()
fn C.v_test_stack_guarantee() int
fn C.v_test_disable_error_dialog()

struct Item { value int }

fn deep(n int) int {
	if n == 0 { return 0 }
	return 1 + deep(n - 1)
}

fn recurse() int {
	assert C.v_test_stack_guarantee() == 1
	return deep(10000000)
}

fn main() {
	C.v_test_disable_error_dialog()
	match os.args[1] {
		"thread" {
			t := spawn recurse()
			println(t.wait())
		}
		"nil" {
			p := unsafe { &Item(nil) }
			println(p.value)
		}
		"first_chance" {
			// Order the handlers as V overflow -> test catcher -> TCC backtrace.
			// TCC otherwise treats this handled first-chance exception as fatal.
			C.v_test_add_exception_handler()
			C.v_install_windows_stack_overflow_handler()
			C.v_test_first_chance_exception()
			println("handled first-chance exception")
		}
		else { println(recurse()) }
	}
}
'

fn test_windows_stack_overflow_in_main_and_spawned_threads() ! {
	$if !windows {
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'windows_stack_overflow_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'child.c.v')
	os.write_file(source, windows_overflow_child_source)!
	os.write_file(os.join_path(dir, 'windows_overflow.h'), windows_overflow_header)!
	for build_mode in ['normal', 'no_backtrace', 'parallel'] {
		binary := os.join_path(dir, 'child_${build_mode}.exe')
		mut flags := [@VEXE, '-new-compiler', '-nocache']
		if build_mode == 'no_backtrace' {
			flags << ['-d', 'no_backtrace']
		} else if build_mode == 'parallel' {
			cc := os.find_abs_path_of_executable('gcc') or { continue }
			flags << ['-parallel-cc', '-cc', cc, '-showcc']
		}
		flags << ['-o', binary, source]
		compile := os.exec(flags)
		assert compile.exit_code == 0, '${build_mode}: ${compile.output}'
		if build_mode == 'parallel' {
			// The opaque helper header cannot prove split safety, so -parallel-cc
			// falls back to one C unit while preserving runtime exception handling.
			assert compile.output.contains('src.c'), compile.output
			assert !compile.output.contains('unit_1.c'), compile.output
		}
		for mode in ['main', 'thread'] {
			result := os.exec([binary, mode])
			assert result.exit_code == 1, '${build_mode}/${mode}: ${result.output}'
			assert result.output.trim_space() == 'V panic: stack overflow', '${build_mode}/${mode}: ${result.output}'
		}
		first_chance := os.exec([binary, 'first_chance'])
		assert first_chance.exit_code == 0, first_chance.output
		assert first_chance.output.trim_space() == 'handled first-chance exception', first_chance.output
		if build_mode == 'normal' {
			fault := os.exec([binary, 'nil'])
			assert fault.exit_code != 0, fault.output
			$if tinyc {
				// TCC's first-chance backtrace handler reports this before V's filter.
				assert fault.output.contains('RUNTIME ERROR: invalid memory access'), fault.output
			} $else {
				assert fault.output.contains('Unhandled Exception'), fault.output
			}
			assert !fault.output.contains('stack overflow'), fault.output
		}
	}
}
