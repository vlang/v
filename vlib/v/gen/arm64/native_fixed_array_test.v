module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_returned_registration_fixed_array_fields_preserve_string_elements() {
	$if macos && arm64 {
		definitions := os.read_file(os.join_path(@VMODROOT, 'vlib', 'v', 'types', 'type.v'))!
		cgen := os.read_file(os.join_path(@VMODROOT, 'vlib', 'v', 'gen', 'c', 'cleanc.v'))!
		registration := 'struct FnSignatureRegistration {' +
			cgen.all_after('struct FnSignatureRegistration {').all_before('\n}') + '\n}\n'
		source := definitions.all_before('// clone_owned_type').replace('module types', 'module main') +
			registration.replace('types.Type', 'Type') + r'
fn C.exit(int)
fn make_registration() FnSignatureRegistration {
    mut aliases := [6]string{}
    aliases[0] = "sum"
    aliases[1] = "builtin.sum"
    aliases[2] = "builtin__sum"
    return FnSignatureRegistration{
        module_key: "builtin:sum"
        short_name: "sum"
        aliases: aliases
        alias_count: 3
    }
}
fn verify_registration(registration FnSignatureRegistration) {
    expected := ["sum", "builtin.sum", "builtin__sum", "", "", ""]!
    keys := map[string]bool{"sum": true, "builtin.sum": true, "builtin__sum": true, "": true}
    for alias_idx in 0 .. 6 {
        alias := registration.aliases[alias_idx]
        if alias.len > 0 && u64(alias.str) < u64(65536) { C.exit(10 + alias_idx) }
        if alias != expected[alias_idx] { C.exit(20 + alias_idx) }
        if alias !in keys || !keys[alias] { C.exit(30 + alias_idx) }
    }
    if registration.module_key != "builtin:sum" || registration.short_name != "sum"
        || registration.alias_count != 3 { C.exit(40) }
}
fn main() {
    registration := make_registration()
    verify_registration(registration)
    copy := registration
    verify_registration(copy)
}
'
		for building_v in [false, true] {
			path := os.join_path(os.vtmp_dir(), 'arm64_registration_aliases_${building_v}_${os.getpid()}.v')
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

fn test_native_mut_fixed_array_parameters_preserve_element_sizes_and_values() {
	$if macos && arm64 {
		source := 'module main
fn C.exit(int)
struct Cache {
mut:
    pointers [4096]voidptr
    values [4096]string
    canary i64
}
fn cache_value(value string, slot int, mut pointers [4096]voidptr, mut values [4096]string) string {
    if pointers[slot] == voidptr(value.str) && values[slot].len == value.len {
        return values[slot]
    }
    pointers[slot] = voidptr(value.str)
    values[slot] = value
    return value
}
fn forward(value string, slot int, mut pointers [4096]voidptr, mut values [4096]string) string {
    return cache_value(value, slot, mut pointers, mut values)
}
fn main() {
    mut cache := Cache{canary: 123456789}
    first := "first string"
    second := "second string"
    if forward(first, 17, mut cache.pointers, mut cache.values) != first { C.exit(1) }
    if forward(second, 18, mut cache.pointers, mut cache.values) != second { C.exit(2) }
    if forward(first, 17, mut cache.pointers, mut cache.values) != first { C.exit(3) }
    if cache.values[17] != first || cache.values[18] != second { C.exit(4) }
    if forward(second, 4095, mut cache.pointers, mut cache.values) != second { C.exit(5) }
    if cache.canary != 123456789 || cache.values[4095] != second { C.exit(6) }
}
'
		path := os.join_path(os.vtmp_dir(), 'arm64_mut_fixed_array_${os.getpid()}.v')
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
		assert result.exit_code == 0, result.output
	}
}
