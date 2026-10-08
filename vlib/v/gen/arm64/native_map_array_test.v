module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_map_array_fields_keep_map_indexing_through_pointer_receivers() {
	$if macos && arm64 {
		source := r'module main
fn C.exit(int)
fn C.malloc(usize) voidptr
fn C.memcpy(voidptr, voidptr, usize) voidptr
fn memdup(source voidptr, size isize) voidptr {
    result := C.malloc(usize(size))
    return C.memcpy(result, source, usize(size))
}
fn array_new(element_size int, len int, cap int) []int { return []int{} }
fn __new_array_noscan(len int, cap int, element_size int) []int {
    return array_new(element_size, len, cap)
}
struct Declarations {
mut:
    ids map[string][]int
}
struct Checker { declarations &Declarations }
fn lookup_key(name string) string { return name }
fn (declarations &Declarations) sum(name string) int {
    mut total := 0
    for index in declarations.ids[lookup_key(name)] { total += index }
    return total
}
fn (checker &Checker) sum(name string) int {
    mut total := 0
    for index in checker.declarations.ids[lookup_key(name)] { total += index }
    return total
}
fn main() {
    mut declarations := Declarations{ids: map[string][]int{}}
    declarations.ids["present"] = [1, 3, 7]
    declarations.ids["empty"] = []int{}
    checker := Checker{declarations: &declarations}
    if declarations.sum("present") != 11 { C.exit(1) }
    if declarations.sum("empty") != 0 { C.exit(2) }
    if declarations.sum("missing") != 0 { C.exit(3) }
    if checker.sum("present") != 11 { C.exit(4) }
    if checker.sum("empty") != 0 || checker.sum("missing") != 0 { C.exit(5) }
    values := declarations.ids["present"]
    if values.len != 3 || values[1] != 3 { C.exit(6) }
}
'
		for building_v in [false, true] {
			path := os.join_path(os.vtmp_dir(), 'arm64_map_array_${building_v}_${os.getpid()}.v')
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
