module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_negative_sum_guards_preserve_enum_payload_fields() {
	$if macos && arm64 {
		type_source := os.read_file(os.join_path(os.dir(@FILE), '..', '..', 'types', 'type.v')) or {
			panic(err)
		}
		// Use the compiler's recursive Type layout and its actual Enum tag.
		source := type_source.all_before('// clone_owned_type').replace('module types', 'module main').replace('import v.flat', '') +
			r'
const builtin_void_type = Type(Void{})
const empty_type = &builtin_void_type
fn C.exit(int)
fn C.malloc(uint) voidptr
fn C.memcpy(voidptr, voidptr, uint) voidptr
fn memdup(value voidptr, size int) voidptr {
    copy := C.malloc(uint(size))
    return C.memcpy(copy, value, uint(size))
}
fn __as_cast(obj voidptr, obj_type int, expected_type int, obj_name string, expected_name string) voidptr {
    if obj_type != expected_type { C.exit(80) }
    return obj
}
fn unalias_type(value &Type) &Type {
    mut current := value
    for current is Alias { current = current.base_type }
    return current
}
fn fixture_type_name(id u32) string {
    return match id {
        1 { "Plain" }
        2 { "Flag" }
        3 { "Listed" }
        4 { "Other" }
        5 { "Aliased" }
        6 { "changed" }
        else { "other" }
    }
}
fn (value &Type) name() string {
    if value is Enum { return fixture_type_name(value.name_id) }
    if value is Struct { return fixture_type_name(value.name_id) }
    return "other"
}
fn needs_else(subject Type, flag_enums map[string]bool, diagnose bool) bool {
    clean_subject := unalias_type(subject)
    if clean_subject !is Enum
        || (!clean_subject.is_flag && clean_subject.name() !in flag_enums)
        || !diagnose { return false }
    return true
}
fn has_flags(subject Type) bool {
    return subject is Enum && subject.is_flag
}
fn negated_guard(subject Type) bool {
    return !(subject !is Enum) && subject.is_flag
}
fn compound_guard(subject Type, diagnose bool) bool {
    return (subject !is Enum || !diagnose) || subject.is_flag
}
fn whole_sum_receiver(subject Type) string {
    if subject !is Enum || subject.is_flag { return "guarded" }
    return subject.name()
}
fn same_function_shape(expected FnType, actual Type) bool {
    if actual !is FnType || expected.params.len != actual.params.len { return false }
    for i in 0 .. expected.params.len {
        if expected.params[i].typ.name() != actual.params[i].typ.name() { return false }
    }
    return actual.return_type.name() == expected.return_type.name()
}
struct TypeChecker {}
fn fn_param_modes_compatible(actual FnType, expected FnType, idx int) bool {
    actual_mut := idx < actual.params.len && actual.params[idx].is_mut
    expected_mut := idx < expected.params.len && expected.params[idx].is_mut
    return actual_mut == expected_mut
}
fn fn_compatible_param_type(value FnType, idx int) Type { return *value.params[idx].typ }
fn (tc &TypeChecker) types_match_ignoring_module_qualification(expected Type, actual Type) bool {
    return expected.name() == actual.name()
}
fn (tc &TypeChecker) fn_signature_return_compatible(actual Type, expected Type) bool {
    return actual.name() == expected.name()
}
fn (tc &TypeChecker) fn_types_match_ignoring_module_qualification(expected FnType, actual Type) bool {
    if actual !is FnType || expected.params.len != actual.params.len {
        return false
    }
    actual_fn := actual as FnType
    for i in 0 .. expected.params.len {
        if !fn_param_modes_compatible(actual_fn, expected, i)
            || !tc.types_match_ignoring_module_qualification(fn_compatible_param_type(expected, i), fn_compatible_param_type(actual_fn, i)) {
            return false
        }
    }
    return tc.fn_signature_return_compatible(actual.return_type, expected.return_type)
}
fn reassign_after_guard(subject Type) string {
    mut current := subject
    if current !is Enum { return "other" }
    current = Struct{name_id: 6}
    if current !is Struct { return "wrong variant" }
    return fixture_type_name(current.name_id)
}
fn branch_join(subject Type, take_branch bool) string {
    if take_branch {
        if subject !is Enum { return "other" }
        if subject.is_flag { return "flag" }
    } else {
        if subject is Enum && subject.is_flag { return "flag" }
    }
    if subject !is Struct { return subject.name() }
    return fixture_type_name(subject.name_id)
}
fn main() {
    flags := map[string]bool{"Listed": true}
    plain := Type(Enum{name_id: 1, is_flag: false})
    flag := Type(Enum{name_id: 2, is_flag: true})
    listed := Type(Enum{name_id: 3, is_flag: false})
    other := Type(Struct{name_id: 4})
    alias := Type(Alias{name_id: 5, base_type: new_type_reference(plain)})
    if needs_else(plain, flags, true) { C.exit(1) }
    if !needs_else(flag, flags, true) { C.exit(2) }
    if !needs_else(listed, flags, true) { C.exit(3) }
    if needs_else(other, flags, true) { C.exit(4) }
    if needs_else(alias, flags, true) { C.exit(5) }
    if needs_else(flag, flags, false) { C.exit(6) }
    if has_flags(plain) || !has_flags(flag) || has_flags(other) { C.exit(7) }
    if negated_guard(plain) || !negated_guard(flag) || negated_guard(other) { C.exit(8) }
    if compound_guard(plain, true) || !compound_guard(flag, true)
        || !compound_guard(other, true) || !compound_guard(plain, false) { C.exit(9) }
    if whole_sum_receiver(plain) != "Plain" || whole_sum_receiver(flag) != "guarded"
        || whole_sum_receiver(other) != "guarded" { C.exit(10) }
    signature := FnType{params: [FnParam{typ: new_type_reference(other)}], return_type: new_type_reference(plain)}
    if !same_function_shape(signature, Type(signature)) { C.exit(11) }
    if same_function_shape(signature, Type(FnType{return_type: new_type_reference(plain)})) { C.exit(12) }
    if same_function_shape(signature, other) { C.exit(13) }
    if same_function_shape(signature, Type(FnType{params: [FnParam{typ: new_type_reference(plain)}], return_type: new_type_reference(plain)})) { C.exit(14) }
    if reassign_after_guard(plain) != "changed" || reassign_after_guard(other) != "other" { C.exit(15) }
    if branch_join(plain, true) != "Plain" || branch_join(flag, true) != "flag"
        || branch_join(other, false) != "Other" || branch_join(other, true) != "other" { C.exit(16) }
    checker := TypeChecker{}
    if !checker.fn_types_match_ignoring_module_qualification(signature, Type(signature)) { C.exit(17) }
    if checker.fn_types_match_ignoring_module_qualification(signature, Type(FnType{return_type: new_type_reference(plain)})) { C.exit(18) }
    if checker.fn_types_match_ignoring_module_qualification(signature, other) { C.exit(19) }
    if checker.fn_types_match_ignoring_module_qualification(signature, Type(FnType{params: [FnParam{typ: new_type_reference(other)}], return_type: new_type_reference(other)})) { C.exit(20) }
    mutable := FnType{params: [FnParam{typ: new_type_reference(other), is_mut: true}], return_type: new_type_reference(plain)}
    if checker.fn_types_match_ignoring_module_qualification(signature, Type(mutable)) { C.exit(21) }
}
'
		for building_v in [false, true] {
			path := os.join_path(os.vtmp_dir(), 'arm64_sum_guard_${building_v}_${os.getpid()}.v')
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
