module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_native_tuple_function_pointers_preserve_register_returns() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_native_tuple_abi_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'callbacks')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'native_tuple_abi' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'native.h'), r'#ifndef FASTC_NATIVE_TUPLE_ABI_H
#define FASTC_NATIVE_TUPLE_ABI_H
#include <stdint.h>
typedef struct { uint64_t a; uint64_t b; } native_pair;
static uint64_t native_context = UINT64_C(0x13579bdf2468ace0);
static void *native_context_pointer(void) { return &native_context; }
static uint64_t native_context_read(void *context) { return *(uint64_t *)context; }
static uint64_t native_invoke_pair(void *callback, void *context, uint64_t argument, uint64_t index) {
    native_pair result = ((native_pair (*)(void *, uint64_t))callback)(context, argument);
    return index == 0 ? result.a : result.b;
}
#endif
') or { panic(err) }
	os.write_file(os.join_path(root, 'callbacks', 'callbacks.v'), r'module callbacks
#include "@DIR/../native.h"
pub type Word = u64
fn C.native_context_read(context voidptr) u64
pub fn pair(context voidptr, argument u64) (Word, u64) {
	return Word(C.native_context_read(context) ^ argument), u64(0xfedcba9876543210) ^ argument
}
pub fn native_only(context voidptr, argument u64) (Word, u64) {
	return Word(C.native_context_read(context) ^ argument), u64(0xfedcba9876543210) ^ argument
}
') or { panic(err) }
	source_path := os.join_path(root, 'main.v')
	os.write_file(source_path, r'module main
import callbacks
#include "@DIR/native.h"
type Word = u64
type PairCallback = fn (voidptr, u64) (Word, u64)
fn C.native_context_pointer() voidptr
fn C.native_context_read(context voidptr) u64
fn C.native_invoke_pair(callback voidptr, context voidptr, argument u64, index u64) u64
fn make_pair(context voidptr, argument u64) (Word, u64) {
	return Word(C.native_context_read(context) ^ argument), u64(0xfedcba9876543210) ^ argument
}
fn small_pair(context voidptr, argument u64) (Word, u64) {
	_ = context
	_ = argument
	return 0, 22
}
fn invoke_typed(callback fn (voidptr, u64) (Word, u64), context voidptr, argument u64) (Word, u64) {
	return callback(context, argument)
}
fn check_native(callback voidptr, context voidptr, argument u64) {
	assert C.native_invoke_pair(callback, context, argument, 0) == (C.native_context_read(context) ^ argument)
	assert C.native_invoke_pair(callback, context, argument, 1) == (u64(0xfedcba9876543210) ^ argument)
}
fn main() {
	context := C.native_context_pointer()
	argument := u64(0x0102030405060708)
	expected_first := C.native_context_read(context) ^ argument
	expected_second := u64(0xfedcba9876543210) ^ argument
	first, second := make_pair(context, argument)
	assert first == Word(expected_first) && second == expected_second, "direct V tuple call"
	callback := PairCallback(make_pair)
	callback_first, callback_second := callback(context, argument)
	assert callback_first == Word(expected_first) && callback_second == expected_second, "typed V tuple callback"
	inline_first, inline_second := invoke_typed(make_pair, context, argument)
	assert inline_first == Word(expected_first) && inline_second == expected_second, "inline V tuple callback parameter"
	qualified_first, qualified_second := callbacks.pair(context, argument)
	assert qualified_first == callbacks.Word(expected_first) && qualified_second == expected_second
	check_native(voidptr(make_pair), context, argument)
	check_native(voidptr(callbacks.pair), context, argument)
	check_native(voidptr(callbacks.native_only), context, argument)
	assert C.native_invoke_pair(voidptr(small_pair), context, argument, 0) == 0, "small tuple first value"
	assert C.native_invoke_pair(voidptr(small_pair), context, argument, 1) == 22, "small tuple second value"
}
') or { panic(err) }
	baseline := os.join_path(root, 'baseline')
	baseline_build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'c', '-gc', 'none', '-o', baseline,
		source_path], @VEXEROOT, {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert baseline_build.exit_code == 0, baseline_build.output
	baseline_run := cmdexec.run(baseline, [])
	assert baseline_run.exit_code == 0, baseline_run.output
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	probe_source := os.join_path(root, 'pattern_probe.c')
	probe_object := os.join_path(root, 'pattern_probe.o')
	os.write_file(probe_source, 'int main(void) { return 0; }\n') or { panic(err) }
	pattern_probe := cmdexec.run('cc', ['-Werror', '-ftrivial-auto-var-init=pattern', '-c',
		probe_source, '-o', probe_object])
	for target_headers in [false, true] {
		prefs.target_libc_headers = target_headers
		source := generate_files([root], prefs) or { panic(err) }
		if !target_headers {
			assert !source.contains('v_fastc_native_pair_u64'), source
			continue
		}
		c_path := os.join_path(root, 'main.c')
		program := os.join_path(root, 'program')
		os.write_file(c_path, source) or { panic(err) }
		mut flags := ['-std=gnu11', '-O2']
		if pattern_probe.exit_code == 0 {
			// Make a narrow tuple store's unwritten upper bytes visible when supported.
			flags << '-ftrivial-auto-var-init=pattern'
		}
		flags << ['-o', program, c_path]
		build := cmdexec.run('cc', flags)
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
	}
}
