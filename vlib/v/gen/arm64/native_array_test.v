module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_cloned_array_fields_survive_return_statement_deferred_restoration() {
	$if macos && arm64 {
		source := r'module main
fn C.exit(int)
struct OwnershipDropEntry {
    name string
    type_name string
    optional_wrapper bool
}
struct Generator {
mut:
    cur_return_node_id int = -1
    ownership_return_index int
    ownership_seen_return_sources map[string]bool
    ownership_propagation_index int
    cur_return_drops []OwnershipDropEntry
    pending_return_scope_drops []OwnershipDropEntry
}
fn (mut g Generator) gen_node(id int, returning bool) {
    if !returning {
        return
    } else {
        old_return_node_id := g.cur_return_node_id
        old_return_drops := g.cur_return_drops.clone()
        g.cur_return_node_id = id
        g.cur_return_drops = [OwnershipDropEntry{name: "temporary", type_name: "int"}]
        defer {
            g.cur_return_node_id = old_return_node_id
            g.cur_return_drops = old_return_drops
        }
        if id % 2 == 0 { return }
        if id % 3 == 0 { return }
    }
}
fn record_order(mut sequence []int, register bool, early bool) {
    defer { sequence << 1 }
    if register { defer(fn) { sequence << 2 } }
    defer { sequence << 3 }
    if early { return }
}
fn main() {
    mut empty := Generator{}
    for id in 1 .. 8 {
        empty.gen_node(id, false)
        if empty.cur_return_node_id != -1 || empty.cur_return_drops.len != 0 { C.exit(1) }
        empty.gen_node(id, true)
        if empty.cur_return_node_id != -1 || empty.cur_return_drops.len != 0 { C.exit(1) }
    }
    mut generator := Generator{
        cur_return_node_id: 9822
        ownership_return_index: 17
        ownership_seen_return_sources: {"seen": true}
        ownership_propagation_index: 19
        cur_return_drops: [OwnershipDropEntry{name: "original", type_name: "string", optional_wrapper: true}]
        pending_return_scope_drops: [OwnershipDropEntry{name: "pending", type_name: "int"}]
    }
    for id in 1 .. 8 {
        generator.gen_node(id, false)
        if generator.cur_return_node_id != 9822 || generator.cur_return_drops.len != 1 { C.exit(2) }
        generator.gen_node(id, true)
        if generator.cur_return_node_id != 9822 || generator.cur_return_drops.len != 1 { C.exit(2) }
        entry := generator.cur_return_drops[0]
        if entry.name != "original" || entry.type_name != "string" || !entry.optional_wrapper { C.exit(3) }
        if generator.ownership_return_index != 17 || generator.ownership_propagation_index != 19
            || !generator.ownership_seen_return_sources["seen"] { C.exit(4) }
        if generator.pending_return_scope_drops.len != 1
            || generator.pending_return_scope_drops[0].name != "pending" { C.exit(5) }
    }
    for register in [false, true] {
        for early in [false, true] {
            mut sequence := [99]
            record_order(mut sequence, register, early)
            if register {
                if sequence.len != 4 || sequence[0] != 99 || sequence[1] != 3 || sequence[2] != 2 || sequence[3] != 1 { C.exit(6) }
            } else {
                if sequence.len != 3 || sequence[0] != 99 || sequence[1] != 3 || sequence[2] != 1 { C.exit(7) }
            }
        }
    }
}
'
		for building_v in [false, true] {
			path := os.join_path(os.vtmp_dir(), 'arm64_array_deferred_clone_${building_v}_${os.getpid()}.v')
			output := path.all_before_last('.')
			defer {
				os.rm(path) or {}
				os.rm(output) or {}
			}
			os.write_file(path, source)!
			mut preferences := pref.new_preferences()
			preferences.backend = 'arm64'
			mut p := parser.Parser.new(preferences)
			mut a := p.parse_file(path)
			assert p.diagnostics.len == 0, p.diagnostics.str()
			mut tc := types.TypeChecker.new(a)
			tc.building_v_fast = building_v
			tc.collect(a)
			if !building_v {
				tc.annotate_types()
			}
			assert tc.errors.len == 0, tc.errors.str()
			if building_v {
				_, _, errors := transform.transform_with_used_opt_config_scoped_workers_checked(mut a, tc, map[string]bool{}, false, true, false, true)
				assert errors.len == 0, errors.str()
			} else {
				transform.transform(mut a, tc)
			}
			m := ssa.build_with_used(a, map[string]bool{}, tc)
			mut g := Gen.new(m)
			g.gen()
			g.write_and_link(output)
			result := os.exec([output])
			assert result.exit_code == 0, 'exit ${result.exit_code}: ${result.output}'
		}
	}
}

fn test_native_array_growth_handles_managed_storage_and_preserves_views() {
	$if macos && arm64 {
		run_native_array_fixture('managed', 'module main
fn C.exit(int)
fn C.malloc(usize) voidptr
fn C.free(voidptr)
fn C.memcpy(voidptr, voidptr, usize) voidptr
struct NativeArray {
    data voidptr
    offset i32
    len i32
    cap i32
    flags u32
    element_size i32
}
fn main() {
    allocation := C.malloc(48)
    data := unsafe { &u8(allocation) + 16 }
    unsafe { *(&voidptr(allocation)) = allocation }
    header := NativeArray{data: data, len: 8, cap: 8, flags: 16, element_size: 4}
    mut values := []int{}
    C.memcpy(&values, &header, sizeof(NativeArray))
    values[0] = 10
    values[1] = 11
    values[2] = 12
    values[3] = 13
    values[4] = 14
    values[5] = 15
    values[6] = 16
    values[7] = 17
    old := values[1..4]
    nested := old[1..3]
    mut nested_header := NativeArray{}
    C.memcpy(&nested_header, &nested, sizeof(NativeArray))
    if nested_header.offset != 8 || nested_header.flags != 80 { C.exit(1) }
    if unsafe { *(&u8(allocation) + 8) } != u8(1) { C.exit(2) }
    values << 18
    if values.len != 9 { C.exit(30) }
    if values[0] != 10 { C.exit(31) }
    if values[8] != 18 { C.exit(32) }
    if old[0] != 11 || old[2] != 13 || nested[0] != 12 { C.exit(4) }
    values[1] = 99
    if old[0] != 11 { C.exit(5) }
    mut grown := NativeArray{}
    C.memcpy(&grown, &values, sizeof(NativeArray))
    if grown.offset != 0 || grown.flags != 16 || usize(grown.data) % 16 != 0 { C.exit(6) }
    if grown.data == header.data { C.exit(7) }
    new_allocation := unsafe { *(&voidptr(&u8(grown.data) - 16)) }
    C.free(new_allocation)
    C.free(allocation)
}
')
	}
}

fn test_native_array_bulk_growth_and_slice_append_preserve_parent_storage() {
	$if macos && arm64 {
		run_native_array_fixture('bulk', 'module main
fn C.exit(int)
fn main() {
    mut values := [11, 22, 33, 44]
    mut tail := values[1..3]
    tail << 55
    if tail.len != 3 { C.exit(10) }
    if tail[0] != 22 { C.exit(11) }
    if tail[1] != 33 { C.exit(12) }
    if tail[2] != 55 { C.exit(13) }
    if values.len != 4 || values[1] != 22 || values[3] != 44 { C.exit(2) }
    tail[0] = 66
    if values[1] != 22 { C.exit(3) }
    values << values
    if values.len != 8 || values[0] != 11 || values[4] != 11 || values[7] != 44 { C.exit(4) }
    mut tail_again := values[5..7]
    tail_again << [77, 88]
    if tail_again.len != 4 || tail_again[0] != 22 || tail_again[1] != 33 { C.exit(5) }
    if tail_again[2] != 77 || tail_again[3] != 88 || values.len != 8 { C.exit(6) }
}
')
	}
}

fn test_native_string_builder_growth_handles_managed_array_backing() {
	$if macos && arm64 {
		run_native_array_fixture('builder', 'module main
fn C.exit(int)
fn C.malloc(usize) voidptr
fn C.memcpy(voidptr, voidptr, usize) voidptr
type Builder = []u8
fn (mut b Builder) write_string(value string) {}
fn (mut b Builder) write_byte(value u8) {}
fn (mut b Builder) write_u8(value u8) {}
fn (mut b Builder) str() string { return "" }
struct NativeArray {
    data voidptr
    offset i32
    len i32
    cap i32
    flags u32
    element_size i32
}
fn main() {
    allocation := C.malloc(18)
    data := unsafe { &u8(allocation) + 16 }
    unsafe {
        *(&voidptr(allocation)) = allocation
        data[0] = u8(97)
        data[1] = u8(98)
    }
    header := NativeArray{data: data, len: 2, cap: 2, flags: 16, element_size: 1}
    mut builder := Builder([]u8{})
    C.memcpy(&builder, &header, sizeof(NativeArray))
    builder.write_string("cd")
    if builder.str() != "abcd" { C.exit(1) }
    builder.write_string("efghijklmnop")
    if builder.str() != "abcdefghijklmnop" { C.exit(2) }
    builder.write_byte(u8(0))
    builder.write_byte(u8(255))
    builder.write_string("Q")
    builder.write_u8(u8(127))
    for index in 0 .. 64 { builder.write_byte(u8(index)) }
    text := builder.str()
    if text.len != 84 || text[..16] != "abcdefghijklmnop" { C.exit(3) }
    if text[16] != u8(0) { C.exit(4) }
    if text[17] != u8(255) { C.exit(5) }
    if text[18] != u8(81) { C.exit(6) }
    if text[19] != u8(127) { C.exit(7) }
    for index in 0 .. 64 {
        if text[20 + index] != u8(index) { C.exit(8) }
    }
    if unsafe { text.str[text.len] } != u8(0) { C.exit(9) }
}
')
	}
}

fn run_native_array_fixture(name string, source string) {
	path := os.join_path(os.vtmp_dir(), 'arm64_array_${name}_${os.getpid()}.v')
	output := path.all_before_last('.')
	defer {
		os.rm(path) or {}
		os.rm(output) or {}
	}
	os.write_file(path, source) or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.annotate_types()
	assert tc.errors.len == 0, tc.errors.str()
	// The full transformer marks array appends before SSA sees the infix node.
	for i, node in a.nodes {
		if node.kind == .infix && node.op == .left_shift && node.children_count == 2 {
			if typ := tc.expr_type(a.child(&node, 0)) {
				if typ.name().starts_with('[]') {
					a.nodes[i].value = 'push'
				}
			}
		}
	}
	m := ssa.build_with_used(a, map[string]bool{}, tc)
	mut g := Gen.new(m)
	g.gen()
	g.write_and_link(output)
	result := os.exec([output])
	assert result.exit_code == 0, result.output
}
