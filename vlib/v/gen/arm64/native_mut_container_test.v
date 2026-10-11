module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_for_in_loads_mutable_container_headers() {
	$if macos && arm64 {
		source := r'module main
fn C.exit(int)
fn array_new(element_size int, len int, cap int) []int { return []int{} }
fn __new_array_noscan(len int, cap int, element_size int) []int {
    return array_new(element_size, len, cap)
}
fn overlaps(mut seen_values map[int]int, low int, high int) int {
    mut duplicates := map[int]int{}
    for value, owner_branch in seen_values {
        if value >= low && value <= high { duplicates[value] = owner_branch }
    }
    mut total := 0
    for _, branch in duplicates { total += branch }
    return total
}
fn array_total(mut values []int) int {
    mut total := 0
    for index, value in values { total += index + value }
    return total
}
fn string_total(mut text string) int {
    mut total := 0
    for index, value in text { total += index + int(value) }
    return total
}
fn main() {
    mut values := map[int]int{}
    values[1] = 4
    values[3] = 7
    values[9] = 11
    if overlaps(mut values, 0, 5) != 11 { C.exit(1) }
    if overlaps(mut values, 20, 30) != 0 { C.exit(2) }
    mut empty_map := map[int]int{}
    if overlaps(mut empty_map, 0, 5) != 0 { C.exit(3) }
    mut numbers := [int(2), int(3), int(5)]
    if array_total(mut numbers) != 13 { C.exit(4) }
    mut empty_array := []int{}
    if array_total(mut empty_array) != 0 { C.exit(5) }
    mut text := "abc"
    if string_total(mut text) != 297 { C.exit(6) }
    mut empty_text := ""
    if string_total(mut empty_text) != 0 { C.exit(7) }
}
'
		for building_v in [false, true] {
			path := os.join_path(os.vtmp_dir(), 'arm64_mut_container_${building_v}_${os.getpid()}.v')
			output := path.all_before_last('.')
			defer {
				os.rm(path) or {}
				os.rm(output) or {}
			}
			os.write_file(path, source) or { panic(err) }
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
