module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_map_index_handles_many_keys_updates_and_numeric_bits() {
	$if macos && arm64 {
		run_native_map_index_fixture('many_keys', r'module main
fn C.exit(int)
fn C.memcpy(voidptr, voidptr, usize) voidptr
fn println(s string) {}
fn int_str(n i64) string { return "" }
fn map__clone(m &map[int]int) map[int]int { return map[int]int{} }
fn map__clear(m &map[int]int) {}
struct NativeMapState {
    keys voidptr
    values voidptr
    cap i64
    len i64
    key_size i64
    value_size i64
    hash_slots voidptr
    hash_cap i64
    indexed_len i64
}
fn main() {
    mut numbers := map[int]int{}
    mut texts := map[string]int{}
    for i := 0; i < 20000; i++ {
        numbers[i] = i * 3 + 1
        texts["key " + int_str(i64(i))] = i + 7
    }
    for i := 0; i < 20000; i++ {
        if numbers[i] != i * 3 + 1 {
            println("numeric mismatch i=" + int_str(i64(i)) + " got=" + int_str(i64(numbers[i])) + " len=" + int_str(i64(numbers.len)))
            C.exit(1)
        }
        if texts["key " + int_str(i64(i))] != i + 7 { C.exit(2) }
    }
    if numbers[20000] != 0 || texts["missing"] != 0 { C.exit(3) }
    if numbers.len != 20000 || texts.len != 20000 {
        println("numbers=" + int_str(i64(numbers.len)) + " texts=" + int_str(i64(texts.len)))
        C.exit(4)
    }
    mut state_pointer := voidptr(0)
    C.memcpy(&state_pointer, &numbers, usize(8))
    state := &NativeMapState(state_pointer)
    if state.hash_cap < 40000 || state.indexed_len != 20000 { C.exit(5) }
    slots := &i64(state.hash_slots)
    mut indexed := 0
    for i := i64(0); i < state.hash_cap; i++ {
        if slots[i] != 0 { indexed++ }
    }
    if indexed != 20000 { C.exit(6) }
    mut alias := numbers
    alias[7] = 77
    if numbers[7] != 77 || numbers.len != 20000 { C.exit(7) }
    mut copy := map__clone(&numbers)
    copy[7] = 99
    if copy[7] != 99 || numbers[7] != 77 { C.exit(8) }
    alias.delete(7)
    alias.delete(19999)
    if numbers[7] != 0 || numbers[19999] != 0 || numbers.len != 19998 { C.exit(9) }
    for i := 0; i < 20000; i++ {
        if i == 7 || i == 19999 { continue }
        if numbers[i] != i * 3 + 1 { C.exit(10) }
    }
    numbers[7] = 707
    if alias[7] != 707 || copy[7] != 99 { C.exit(11) }
    map__clear(&numbers)
    if alias.len != 0 || alias[7] != 0 || copy.len != 20000 { C.exit(12) }
    alias[3] = 333
    if numbers[3] != 333 || numbers.len != 1 { C.exit(13) }
    mut bits := map[u64]i64{}
    bits[u64(0)] = 1
    bits[u64(9223372036854775808)] = 2
    bits[u64(18446744073709551615)] = 3
    if bits[u64(0)] != 1 || bits[u64(9223372036854775808)] != 2 || bits[u64(18446744073709551615)] != 3 { C.exit(14) }
    mut signed := map[i64]int{}
    signed[i64(-9223372036854775807) - 1] = 4
    signed[i64(9223372036854775807)] = 5
    if signed[i64(-9223372036854775807) - 1] != 4 || signed[i64(9223372036854775807)] != 5 { C.exit(15) }
}
')
	}
}

fn test_native_map_index_resolves_collisions_and_hashes_string_content_and_length() {
	$if macos && arm64 {
		run_native_map_index_fixture('collisions', r'module main
fn C.exit(int)
fn println(s string) {}
fn wyhash(key voidptr, len i64, seed i64, secret &u64) u64 { return 0 }
fn int_str(n i64) string { return "" }
fn main() {
    mut keys := [4]i64{}
    mut count := 0
    for candidate := i64(0); candidate < 1000; candidate++ {
        if u64(wyhash(&candidate, 8, 0, &u64(0))) & 15 == 0 {
            keys[count] = candidate
            count++
            if count == 4 { break }
        }
    }
    if count != 4 { println("collision count=" + int_str(i64(count))); C.exit(1) }
    mut collisions := map[i64]int{}
    for i := 0; i < 4; i++ { collisions[keys[i]] = i + 1 }
    for i := 0; i < 4; i++ {
        if collisions[keys[i]] != i + 1 {
            println("collision mismatch i=" + int_str(i64(i)) + " got=" + int_str(i64(collisions[keys[i]])))
            C.exit(2)
        }
    }
    collisions[keys[1]] = 99
    if collisions[keys[1]] != 99 || collisions.len != 4 { C.exit(3) }
    collisions.delete(keys[0])
    if collisions[keys[0]] != 0 || collisions[keys[1]] != 99 || collisions[keys[3]] != 4 { C.exit(4) }
    collisions[keys[0]] = 88
    if collisions[keys[0]] != 88 || collisions[keys[2]] != 3 { C.exit(5) }
    mut strings := map[string]int{}
    first := "same" + " bytes"
    second := "same bytes"
    strings[first] = 11
    strings[second] = 22
    strings[""] = 33
    embedded := "a\0b"
    if embedded.len != 3 { C.exit(7) }
    strings[embedded] = 44
    strings["a"] = 55
    if strings.len != 4 || strings[first] != 22 || strings[second] != 22 { C.exit(6) }
    if strings[""] != 33 || strings["a\0b"] != 44 || strings["a"] != 55 { C.exit(7) }
}
')
	}
}

fn run_native_map_index_fixture(name string, source string) {
	path := os.join_path(os.vtmp_dir(), 'arm64_map_index_${name}_${os.getpid()}.v')
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
	transform.transform(mut a, tc)
	m := ssa.build_with_used(a, map[string]bool{}, tc)
	mut g := Gen.new(m)
	g.gen()
	g.write_and_link(output)
	result := os.exec([output])
	assert result.exit_code == 0, 'fixture ${name}: exit ${result.exit_code}: ${result.output}'
}
