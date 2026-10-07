module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.types

fn test_native_aligned_memdup_copies_bytes_and_honors_alignment() {
	$if macos && arm64 {
		run_native_allocation_fixture('aligned', 'module main
fn C.exit(int)
fn C.free(voidptr)
fn v3_aligned_memdup(src voidptr, size i64, alignment u64) voidptr { return unsafe { nil } }
fn main() {
    mut source := [4]u8{}
    source[0] = 1
    source[1] = 2
    source[2] = 3
    source[3] = 4
    copy := &u8(v3_aligned_memdup(&source[0], 4, 64))
    if usize(copy) == 0 || usize(copy) % 64 != 0 { C.exit(1) }
    unsafe {
        if copy[0] != 1 || copy[1] != 2 || copy[2] != 3 || copy[3] != 4 { C.exit(2) }
        copy[1] = 9
    }
    if source[1] != 2 { C.exit(3) }
    C.free(copy)
    small := v3_aligned_memdup(&source[0], 4, 1)
    if usize(small) == 0 || usize(small) % 8 != 0 { C.exit(4) }
    C.free(small)
    invalid := v3_aligned_memdup(&source[0], 4, 24)
    if usize(invalid) != 0 { C.exit(5) }
}
')
	}
}

fn test_native_heap_array_copies_header_and_retains_data_view() {
	$if macos && arm64 {
		run_native_allocation_fixture('array', 'module main
fn C.exit(int)
fn C.free(voidptr)
fn v3_heap_array(value []i64) &[]i64 { return unsafe { nil } }
fn main() {
    mut values := [i64(11), i64(22)]
    mut boxed := v3_heap_array(values)
    if boxed.len != 2 || boxed[0] != 11 || boxed[1] != 22 { C.exit(1) }
    values[0] = 33
    if boxed[0] != 33 { C.exit(2) }
    unsafe { boxed.len = 1 }
    if values.len != 2 || boxed.len != 1 { C.exit(3) }
    C.free(boxed)
}
')
	}
}

fn test_native_void_pointer_tables_use_pointer_sized_slots() {
	$if macos && arm64 {
		run_native_allocation_fixture('pointers', 'module main
fn C.exit(int)
fn pointer_slots(data voidptr) &voidptr {
    return unsafe { &voidptr(data) }
}
fn main() {
    mut values := [2]voidptr{}
    mut slots := pointer_slots(&values[0])
    unsafe {
        slots[0] = voidptr(11)
        slots[1] = voidptr(22)
    }
    if usize(values[0]) != 11 || usize(values[1]) != 22 { C.exit(1) }
}
')
	}
}

fn run_native_allocation_fixture(name string, source string) {
	path := os.join_path(os.vtmp_dir(), 'arm64_allocation_${name}_${os.getpid()}.v')
	output := path.all_before_last('.')
	defer {
		os.rm(path) or {}
		os.rm(output) or {}
	}
	os.write_file(path, source) or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.annotate_types()
	assert tc.errors.len == 0, tc.errors.str()
	m := ssa.build_with_used(a, map[string]bool{}, tc)
	mut g := Gen.new(m)
	g.gen()
	g.write_and_link(output)
	result := os.exec([output])
	assert result.exit_code == 0, '${name}: exit ${result.exit_code}: ${result.output}'
}
