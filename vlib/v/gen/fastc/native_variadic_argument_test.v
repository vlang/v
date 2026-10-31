module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_native_variadic_arguments_preserve_borrowed_pointer_calls() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_native_variadic_argument_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'sink.h'), r'#include <stdint.h>
void expect_pointer(void *pointer);
void record_pointer(void *pointer, uint64_t offset);
uint64_t sink(char *format, ...);
uint32_t recorded_calls(void);
uint64_t recorded_offsets(void);
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.c.v'), r'module main
#include "sink.h"
fn C.expect_pointer(voidptr)
fn C.record_pointer(voidptr, u64)
fn C.sink(format charptr, ...voidptr) u64
fn C.recorded_calls() u32
fn C.recorded_offsets() u64
struct Unit { value u32 }
fn read32(unit &Unit, offset u64) u32 {
    C.record_pointer(voidptr(unit), offset)
    return unit.value + u32(offset)
}
fn probe(mut unit Unit) u64 {
    assert read32(unit, 1) == 36
    first := C.sink(c"first", u64(read32(unit, 2)), u64(read32(unit, 3)))
    second := C.sink(c"second", u64(read32(unit, 4)), u64(read32(unit, 5)))
    return first + second
}
@[export: "native_variadic_argument_entry"]
fn entry() i32 {
    mut unit := Unit{value: 35}
    C.expect_pointer(voidptr(&unit))
    assert probe(mut unit) == 154
    assert C.recorded_calls() == 5
    assert C.recorded_offsets() == 62
    return 42
}
fn main() {}
') or { panic(err) }
	native_c := os.join_path(root, 'native.c')
	os.write_file(native_c, r'#include "sink.h"
#include <stdarg.h>
#include <stdlib.h>
static void *expected_pointer;
static uint32_t calls;
static uint64_t offsets;
void expect_pointer(void *pointer) { expected_pointer = pointer; calls = 0; offsets = 0; }
void record_pointer(void *pointer, uint64_t offset) {
    if (pointer != expected_pointer || offset < 1 || offset > 5 || (offsets & (1ULL << offset))) abort();
    calls++; offsets |= 1ULL << offset;
}
uint32_t recorded_calls(void) { return calls; }
uint64_t recorded_offsets(void) { return offsets; }
uint64_t sink(char *format, ...) {
    va_list args; va_start(args, format);
    uint64_t first = va_arg(args, uint64_t), second = va_arg(args, uint64_t);
    va_end(args); return first + second;
}
extern int32_t native_variadic_argument_entry(void);
__attribute__((constructor)) static void verify_arguments(void) {
    if (native_variadic_argument_entry() != 42) abort();
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	for target_libc in [false, true] {
		prefs.target_libc_headers = target_libc
		c_source := generate_files([root], prefs) or { panic(err) }
		program_c := os.join_path(root, 'program.c')
		os.write_file(program_c, c_source) or { panic(err) }
		program := os.join_path(root, 'program')
		build := cmdexec.run('cc', ['-std=gnu11', '-Werror=implicit-function-declaration', program_c,
			native_c, '-o', program])
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
	}
}
