module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_optional_tuple_guards_preserve_both_strings() {
	$if macos && arm64 {
		source := r'module main
fn C.exit(int)
const static_type_method_name_marker = "@static@"
fn (value string) index(needle string) ?int {
    for start in 0 .. value.len {
        if start + needle.len <= value.len && value[start .. start + needle.len] == needle {
            return start
        }
    }
    return none
}
fn decode_static_type_method_name(name string) ?(string, string) {
    marker := name.index(static_type_method_name_marker) or { return none }
    method_start := marker + static_type_method_name_marker.len
    if marker == 0 || method_start >= name.len { return none }
    return name[..marker], name[method_start..]
}
fn result_pair() !(string, int) { return "kept", 7 }
fn plain_pair() (string, string) { return "first", "second" }
fn main() {
    encoded := "worker.Parser@static@new"
    receiver, method := decode_static_type_method_name(encoded) or { C.exit(1) return }
    if receiver != "worker.Parser" || method != "new" { C.exit(2) }
    receiver_name := if recv, _ := decode_static_type_method_name(encoded) { recv } else { "" }
    if receiver_name != "worker.Parser" { C.exit(3) }
    if _, _ := decode_static_type_method_name("invalid") { C.exit(4) }
    if _, _ := decode_static_type_method_name("@static@new") { C.exit(5) }
    if _, _ := decode_static_type_method_name("Parser@static@") { C.exit(6) }
    if _, method_only := decode_static_type_method_name(encoded) {
        if method_only != "new" { C.exit(7) }
    } else { C.exit(8) }
    label, number := result_pair() or { C.exit(9) return }
    if label != "kept" || number != 7 { C.exit(10) }
    first, second := plain_pair()
    if first != "first" || second != "second" { C.exit(11) }
}
'
		for building_v in [false, true] {
			path := os.join_path(os.vtmp_dir(), 'arm64_tuple_option_${building_v}_${os.getpid()}.v')
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
