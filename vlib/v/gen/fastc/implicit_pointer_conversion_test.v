module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_implicit_voidptr_conversions_preserve_pointer_storage() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_implicit_pointer_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'native.h'), r'#include <stdint.h>
static uint64_t native_words[] = { UINT64_C(0x2052545020445352), UINT64_C(0xfedcba9876543210) };
static void *native_record(void) { return native_words; }
static uint64_t native_pointer_bits(void *value) { return (uint64_t)(uintptr_t)value; }
') or { panic(err) }
	source_path := os.join_path(root, 'main.v')
	os.write_file(source_path, r'module main
#include "@DIR/native.h"
struct Record {
	signature u64
	payload u64
}
type RecordRef = &Record
type OtherRecordRef = RecordRef
__global (
	global_record &Record
	global_alias RecordRef
)
fn C.native_record() voidptr
fn C.native_pointer_bits(value voidptr) u64
fn pointer_bits(value &Record) u64 { return C.native_pointer_bits(value) }
fn alias_bits(value RecordRef) u64 { return C.native_pointer_bits(value) }
fn nested_alias_bits(value OtherRecordRef) u64 { return C.native_pointer_bits(value) }
fn main() {
	raw := C.native_record()
	expected := C.native_pointer_bits(raw)
	mut pointer := unsafe { &Record(nil) }
	pointer = unsafe { raw }
	assert C.native_pointer_bits(pointer) == expected, "local pointer assignment"
	assert pointer.signature == 0x2052545020445352
	assert pointer.payload == 0xfedcba9876543210
	global_record = unsafe { raw }
	assert C.native_pointer_bits(global_record) == expected, "global pointer assignment"
	mut alias_pointer := RecordRef(pointer)
	alias_pointer = unsafe { raw }
	assert C.native_pointer_bits(alias_pointer) == expected, "local pointer alias assignment"
	global_alias = unsafe { raw }
	assert C.native_pointer_bits(global_alias) == expected, "global pointer alias assignment"
	mut nested_alias := OtherRecordRef(alias_pointer)
	nested_alias = unsafe { raw }
	assert C.native_pointer_bits(nested_alias) == expected, "nested pointer alias assignment"
	assert pointer_bits(unsafe { raw }) == expected, "pointer argument"
	assert alias_bits(unsafe { raw }) == expected, "pointer alias argument"
	assert nested_alias_bits(unsafe { raw }) == expected, "nested pointer alias argument"
}
') or { panic(err) }
	baseline := os.join_path(root, 'baseline')
	baseline_build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'c', '-enable-globals', '-gc',
		'none', '-o', baseline, source_path], @VEXEROOT, {
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
	prefs.enable_globals = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	for target_headers in [false, true] {
		prefs.target_libc_headers = target_headers
		source := generate_files([root], prefs) or { panic(err) }
		c_path := os.join_path(root, 'main.c')
		program := os.join_path(root, 'program')
		os.write_file(c_path, source) or { panic(err) }
		build := cmdexec.run('cc', ['-std=gnu11', '-Werror=incompatible-pointer-types', '-O2',
			'-o', program, c_path])
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, 'target headers ${target_headers}: ${run.output}'
	}
}
