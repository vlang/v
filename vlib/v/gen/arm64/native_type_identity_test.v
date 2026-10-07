module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_compiler_type_identity_uses_the_active_payload_slot() {
	$if macos && arm64 {
		type_source := os.read_file(os.join_path(@VMODROOT, 'vlib', 'v', 'types', 'type.v'))!
		checker := os.read_file(os.join_path(@VMODROOT, 'vlib', 'v', 'types', 'checker_tail_stmt.v'))!
		source := type_source.all_before('// clone_owned_type').replace('module types', 'module main') +
			'\nfn type_value_words(' + checker.all_after('fn type_value_words(').all_before('// type_cache_stats') +
			r'
fn C.exit(int)
fn C.malloc(usize) voidptr
fn C.calloc(usize, usize) voidptr
fn C.memcpy(voidptr, voidptr, usize) voidptr
fn memdup(value voidptr, size isize) voidptr {
    result := C.malloc(usize(size))
    return C.memcpy(result, value, usize(size))
}
fn main() {
    integer := Type(Primitive{props: .integer, size: 32})
    boolean := Type(Primitive{props: .boolean, size: 8})
    copied_integer := integer
    integer_tag, integer_payload, integer_slot := type_value_words(&integer)
    boolean_tag, boolean_payload, _ := type_value_words(&boolean)
    copy_tag, copy_payload, copy_slot := type_value_words(&copied_integer)
    if integer_tag != boolean_tag || integer_payload == 0 || boolean_payload == 0
        || integer_payload == boolean_payload { C.exit(1) }
    if copy_tag != integer_tag || copy_payload != integer_payload || copy_slot != integer_slot { C.exit(2) }
    array := Type(Array{elem_type: integer})
    optional_array := Type(OptionType{base_type: array})
    tuple := Type(MultiReturn{types: [integer, array, boolean]})
    optional_tuple := Type(OptionType{base_type: tuple})
    copied_optional := optional_array
    array_tag, array_payload, array_slot := type_value_words(&optional_array)
    tuple_tag, tuple_payload, _ := type_value_words(&optional_tuple)
    copied_tag, copied_payload, copied_slot := type_value_words(&copied_optional)
    if array_tag != tuple_tag || array_payload == 0 || tuple_payload == 0
        || array_payload == tuple_payload { C.exit(3) }
    if copied_tag != array_tag || copied_payload != array_payload || copied_slot != array_slot { C.exit(4) }
    first := Type(FnType{params: [integer], return_type: boolean})
    second := Type(FnType{params: [boolean], return_type: boolean})
    first_tag, first_payload, _ := type_value_words(&first)
    second_tag, second_payload, _ := type_value_words(&second)
    if first_tag != second_tag || first_payload == 0 || first_payload == second_payload { C.exit(5) }
    zero := unsafe { &Type(C.calloc(1, usize(sizeof(Type)))) }
    zero_tag, zero_payload, zero_slot := type_value_words(zero)
    if zero_tag != 0 || zero_payload != 0 || zero_slot != 0 { C.exit(6) }
}
'
		for building_v in [false, true] {
			path := os.join_path(os.vtmp_dir(), 'arm64_type_identity_${building_v}_${os.getpid()}.v')
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
