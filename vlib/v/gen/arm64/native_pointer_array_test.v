module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_c_opaque_pointer_locals_and_explicit_pointer_addresses() {
	$if macos && arm64 {
		for building_v in [false, true] {
			path := os.join_path(os.vtmp_dir(), 'arm64_c_pointer_local_${building_v}_${os.getpid()}.v')
			output := path.all_before_last('.')
			defer {
				os.rm(path) or {}
				os.rm(output) or {}
			}
			os.write_file(path, "module main
fn C.exit(int)
fn C.strtol(&char, &&char, int) i64
fn main() {
    mut ends := [&char(unsafe { nil }), &char(unsafe { nil })]
    target := ends.data
    if C.strtol(c'123x', target, 10) != 123 { C.exit(1) }
    if ends[0] == &char(unsafe { nil }) { C.exit(2) }
    if unsafe { *ends[0] } != char(120) { C.exit(3) }
    if ends[1] != &char(unsafe { nil }) { C.exit(4) }
    mut direct := &char(unsafe { nil })
    if C.strtol(c'456y', &direct, 10) != 456 { C.exit(5) }
    if direct == &char(unsafe { nil }) || unsafe { *direct } != char(121) { C.exit(6) }
}
")!
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
				_, _, errors := transform.transform_with_used_opt_config_scoped_workers_checked(mut a,
					tc, map[string]bool{}, false, true, false, true)
				assert errors.len == 0, errors.str()
			} else {
				transform.transform(mut a, tc)
			}
			m := ssa.build_with_used(a, map[string]bool{}, tc)
			mut g := Gen.new(m)
			g.gen()
			g.write_and_link(output)
			result := os.exec([output])
			assert result.exit_code == 0, 'building_v=${building_v}: ${result.exit_code}: ${result.output}'
		}
	}
}

fn test_native_pointer_elements_and_nil_terminators_keep_their_storage_addresses() {
	$if macos && arm64 {
		source := 'module main
fn C.exit(int)
fn C.malloc(usize) voidptr
fn C.memcpy(voidptr, voidptr, usize) voidptr
fn memdup(source voidptr, size isize) voidptr {
    destination := C.malloc(usize(size))
    return C.memcpy(destination, source, usize(size))
}
fn main() {
    mut first := u8(65)
    mut second := u8(66)
    mut arguments := []&char{cap: 1}
    arguments << &char(&first)
    arguments << &char(&second)
    arguments << &char(unsafe { nil })
    if arguments.len != 3 { C.exit(1) }
    if arguments[0] != &char(&first) || arguments[1] != &char(&second) { C.exit(2) }
    if arguments[2] != &char(unsafe { nil }) { C.exit(3) }
    if unsafe { *arguments[0] } != char(65) || unsafe { *arguments[1] } != char(66) { C.exit(4) }
    mut environment := []voidptr{cap: 1}
    original := voidptr(&first)
    environment << original
    environment << voidptr(unsafe { nil })
    if environment.len != 2 || environment[0] != original { C.exit(5) }
    if environment[1] != voidptr(unsafe { nil }) { C.exit(6) }
    mut copied := voidptr(unsafe { nil })
    C.memcpy(&copied, &original, sizeof(voidptr))
    if copied != original { C.exit(7) }
    pointers := {"first": original}
    if pointers["first"] != original { C.exit(8) }
    if pointers["missing"] != voidptr(unsafe { nil }) { C.exit(9) }
}
'
		path := os.join_path(os.vtmp_dir(), 'arm64_pointer_array_${os.getpid()}.v')
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
		assert result.exit_code == 0, 'exit ${result.exit_code}: ${result.output}'
	}
}
