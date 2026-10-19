module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_explicit_pointer_casts_preserve_address_bits() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_pointer_cast_bits_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'native.h'), r'#include <stdint.h>
static inline void *sentinel_pointer(void) { return (void *)(uintptr_t)0x4015c000; }
static inline uint64_t pointer_bits(void *pointer) { return (uint64_t)(uintptr_t)pointer; }
') or { panic(err) }
	source_path := os.join_path(root, 'main.v')
	os.write_file(source_path, r'module main
import strings
#include "@DIR/native.h"
type Address = u64
type WordRef = &u64
type BlockRef = &Block
type Opaque = voidptr
struct Block { value u64 }
struct Holder { pointer &u64 }
fn C.sentinel_pointer() voidptr
fn C.pointer_bits(pointer voidptr) u64
fn pointer_integer(pointer &u64) u64 { return unsafe { u64(pointer) } }
fn pointer_size(pointer &u64) usize { return unsafe { usize(pointer) } }
fn pointer_alias_integer(pointer WordRef) Address { return unsafe { Address(pointer) } }
fn pointer_alias_opaque(pointer WordRef) Opaque { return unsafe { Opaque(pointer) } }
fn native_alias_pointer() BlockRef { return unsafe { BlockRef(C.sentinel_pointer()) } }
fn returned_pointer() &u64 { return unsafe { &u64(C.sentinel_pointer()) } }
fn borrowed_scalar[T](mut value T) u64 { return unsafe { u64(value) } }
fn borrowed_size[T](mut value T) usize { return unsafe { usize(value) } }
fn borrowed_opaque[T](mut value T) voidptr { return unsafe { voidptr(value) } }
fn borrowed_scalar_pointer[T](mut value T) &u8 { return unsafe { &u8(value) } }
fn borrowed_scalar_value[T](mut value T) u64 { return unsafe { u64(*value) } }
fn borrowed_pointer(mut pointer WordRef) u64 { return unsafe { u64(pointer) } }
fn borrowed_aggregate(mut value Block) u64 { return unsafe { u64(value) } }
fn borrowed_array(mut value []u8) &[]u8 { return unsafe { &[]u8(value) } }
fn borrowed_array_opaque(mut value []u8) voidptr { return unsafe { voidptr(value) } }
fn borrowed_builder(mut value strings.Builder) &[]u8 { return unsafe { &[]u8(value) } }
fn borrowed_map(mut value map[u64]u64) voidptr { return unsafe { voidptr(value) } }
fn borrowed_map_integer(mut value map[u64]u64) u64 { return unsafe { u64(value) } }
fn main() {
	pointer := returned_pointer()
	expected := C.pointer_bits(C.sentinel_pointer())
	assert pointer_integer(pointer) == expected
	assert pointer_size(pointer) == usize(expected)
	assert pointer_alias_integer(WordRef(pointer)) == Address(expected)
	assert C.pointer_bits(pointer_alias_opaque(WordRef(pointer))) == expected
	assert C.pointer_bits(native_alias_pointer()) == expected
	mut pointer_alias := WordRef(pointer)
	assert borrowed_pointer(mut pointer_alias) == C.pointer_bits(voidptr(&pointer_alias))
	holder := Holder{pointer: pointer}
	assert unsafe { u64(holder.pointer) } == expected
	assert unsafe { u64(returned_pointer()) } == expected
	mut value := u64(73)
	value_address := C.pointer_bits(voidptr(&value))
	assert borrowed_scalar(mut value) == value_address
	assert borrowed_size(mut value) == usize(value_address)
	assert C.pointer_bits(borrowed_opaque(mut value)) == value_address
	assert C.pointer_bits(borrowed_scalar_pointer(mut value)) == value_address
	assert borrowed_scalar_value(mut value) == 73
	mut block := Block{value: 91}
	assert borrowed_aggregate(mut block) == C.pointer_bits(voidptr(&block))
	mut values := [u8(1), 2]
	array_address := C.pointer_bits(voidptr(&values))
	assert C.pointer_bits(borrowed_array(mut values)) == array_address
	assert C.pointer_bits(borrowed_array_opaque(mut values)) == array_address
	mut builder := strings.new_builder(8)
	assert C.pointer_bits(borrowed_builder(mut builder)) == C.pointer_bits(voidptr(&builder))
	unsafe { builder.free() }
	mut mapped := map[u64]u64{}
	map_address := C.pointer_bits(voidptr(&mapped))
	assert C.pointer_bits(borrowed_map(mut mapped)) == map_address
	assert borrowed_map_integer(mut mapped) == map_address
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
	for target_headers in [false, true] {
		prefs.target_libc_headers = target_headers
		source := generate_files([root], prefs) or { panic(err) }
		c_path := os.join_path(root, 'main.c')
		program := os.join_path(root, 'program')
		os.write_file(c_path, source) or { panic(err) }
		build := cmdexec.run('cc', ['-std=gnu11', '-O2', '-o', program, c_path])
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
	}
}
