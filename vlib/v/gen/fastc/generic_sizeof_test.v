module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_unbound_generic_sizeof_uses_the_existing_diagnostic_body() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	// A source without main retains every definition, so this checks the actual
	// erased body rather than relying on dead-code elimination to hide it.
	source := generate(r'module main
fn element_bytes[T](values []T) usize { return sizeof(T) }
fn nested_bytes[T](values []T) usize { return 2 * sizeof([3]&T) }
', 'unbound_generic_sizeof.v', prefs) or { panic(err) }
	assert !source.contains('sizeof(T)'), source
	assert !source.contains('sizeof([3]&T)'), source
	assert source.count('was called without a concrete instance') == 2, source
}

fn test_fastc_imported_mutable_generic_array_sizeof_instances_execute() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_generic_sizeof_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'storage')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'storage', 'storage.v'), r'module storage
pub struct Charge {
pub mut:
    bytes u64
}
pub fn reserve[T](mut values []T, mut charge Charge, required int) ? {
    if required < 0 { return none }
    element_bytes := u64(sizeof(T))
    charge.bytes += u64(required) * element_bytes
    values.ensure_cap(required)
}
pub struct Cache {
pub mut:
    charge Charge
}
pub fn (mut cache Cache) reserve[T](mut values []T, required int) ? {
    reserve(mut values, mut cache.charge, required)?
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r'module main
import storage
struct Item { a u64 b u64 }
struct Record {
mut:
    words []u16
    pointers []&Item
    charge storage.Charge
}
@[export: "native_generic_sizeof_entry"]
fn entry() i32 {
    mut record := Record{}
    storage.reserve(mut record.words, mut record.charge, 9) or { return -1 }
    storage.reserve(mut record.pointers, mut record.charge, 7) or { return -2 }
    assert record.words.cap >= 9 && record.pointers.cap >= 7
    assert record.charge.bytes == 9 * 2 + 7 * u64(sizeof(&Item))
    mut words := []u16{}
    mut charge := storage.Charge{}
    storage.reserve[u16](mut words, mut charge, 11) or { return -3 }
    assert words.cap >= 11 && charge.bytes == 22
    mut cache := storage.Cache{}
    cache.reserve(mut words, 13) or { return -4 }
    assert words.cap >= 13 && cache.charge.bytes == 26
    storage.reserve(mut record.words, mut record.charge, -1) or {
        assert record.charge.bytes == 9 * 2 + 7 * u64(sizeof(&Item))
        return 42
    }
    return -5
}
fn main() {}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	for target_libc in [false, true] {
		prefs.target_libc_headers = target_libc
		c_source := generate_files([root], prefs) or { panic(err) }
		assert !c_source.contains('sizeof(T)'), c_source
		assert c_source.contains('sizeof(u16)'), c_source
		assert c_source.contains('sizeof(Item*)'), c_source
		program_c := os.join_path(root, 'program.c')
		native_c := os.join_path(root, 'native.c')
		os.write_file(program_c, c_source) or { panic(err) }
		os.write_file(native_c, r'#include <stdint.h>
#include <stdlib.h>
extern int32_t native_generic_sizeof_entry(void);
__attribute__((constructor)) static void verify_generics(void) {
    if (native_generic_sizeof_entry() != 42) abort();
}
') or { panic(err) }
		program := os.join_path(root, 'program')
		build := cmdexec.run('cc', ['-std=gnu11', '-Werror=implicit-function-declaration', program_c,
			native_c, '-o', program])
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
	}
}
