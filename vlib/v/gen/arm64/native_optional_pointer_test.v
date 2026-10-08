module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_optional_pointer_literals_and_thread_results_preserve_the_pointer() {
	$if macos && arm64 {
		source := r'module main
import prepared
fn C.exit(int)
fn C.malloc(usize) voidptr
fn C.memcpy(voidptr, voidptr, usize) voidptr
fn memdup(source voidptr, size isize) voidptr {
    destination := C.malloc(usize(size))
    C.memcpy(destination, source, usize(size))
    return destination
}
fn select_prepared(shared bool) &prepared.Prepared {
    worker := spawn prepared.make()
    mut waited := ?&prepared.Prepared(none)
    if shared { waited = worker.wait() }
    mut selected := if value := waited { value } else { worker.wait() }
    selected.counts["kept"] = 23
    return selected
}
fn check(shared bool) {
    value := select_prepared(shared)
    if value.counts["kept"] != 23 { C.exit(1) }
    if value.roots.len != 2 || value.roots[0] != "first" || value.roots[1] != "last" { C.exit(2) }
    if value.ready != true { C.exit(3) }
}
fn main() { check(false) check(true) }
'
		module_source := r'module prepared
@[heap]
pub struct Prepared {
pub mut:
    counts map[string]int
    roots []string
    ready bool
}
pub fn make() &Prepared {
    return &Prepared{counts: map[string]int{"kept": 7}, roots: ["first", "last"], ready: true}
}
'
		for building_v in [false, true] {
			path := os.join_path(os.vtmp_dir(), 'arm64_optional_pointer_${building_v}_${os.getpid()}.v')
			module_path := path.all_before_last('.') + '_prepared.v'
			output := path.all_before_last('.')
			defer {
				os.rm(path) or {}
				os.rm(module_path) or {}
				os.rm(output) or {}
			}
			os.write_file(path, source) or { panic(err) }
			os.write_file(module_path, module_source) or { panic(err) }
			mut preferences := pref.new_preferences()
			preferences.backend = 'arm64'
			mut p := parser.Parser.new(preferences)
			mut a := p.parse_files([path, module_path])
			assert p.diagnostics.len == 0, p.diagnostics.str()
			mut tc := types.TypeChecker.new(a)
			tc.building_v_fast = building_v
			tc.collect(a)
			if !building_v { tc.annotate_types() }
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
