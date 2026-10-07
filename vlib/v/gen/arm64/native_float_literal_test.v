module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_real_float_unions_share_storage_before_and_after_transform() {
	$if macos && arm64 {
		source := r'module main
import strconv
fn C.exit(int)
fn C.malloc(usize) voidptr
fn C.memcpy(voidptr, voidptr, usize) voidptr
fn memdup(value voidptr, size isize) voidptr {
    return C.memcpy(C.malloc(usize(size)), value, usize(size))
}
fn bits64(value strconv.Float64u) u64 { return unsafe { value.u } }
fn main() {
    if sizeof(strconv.Float64u) != 8 || sizeof(strconv.Float32u) != 4 { C.exit(1) }
    mut zero64 := strconv.Float64u{}
    if unsafe { zero64.u } != 0 { C.exit(2) }
    zero64.u = u64(0x3ff4000000000000)
    if unsafe { zero64.f } != 1.25 { C.exit(3) }
    from_bits := strconv.Float64u{u: u64(0x408f400000000000)}
    if unsafe { from_bits.f } != 1000.0 { C.exit(4) }
    from_float := strconv.Float64u{f: -2.5}
    if bits64(from_float) != u64(0xc004000000000000) { C.exit(5) }
    heap64 := &strconv.Float64u{u: u64(0x3fe0000000000000)}
    if unsafe { heap64.f } != 0.5 { C.exit(6) }
    mut zero32 := strconv.Float32u{}
    zero32.u = u32(0x3fa00000)
    if unsafe { zero32.f } != f32(1.25) { C.exit(7) }
    from32 := strconv.Float32u{f: f32(-2.5)}
    if unsafe { from32.u } != u32(0xc0200000) { C.exit(8) }
    heap32 := &strconv.Float32u{u: u32(0x3f000000)}
    if unsafe { heap32.f } != f32(0.5) { C.exit(9) }
}
'
		for mode in 0 .. 3 {
			path := os.join_path(os.vtmp_dir(), 'arm64_float_union_${os.getpid()}_${mode}.v')
			output := path.all_before_last('.')
			defer {
				os.rm(path) or {}
				os.rm(output) or {}
			}
			os.write_file(path, source)!
			mut preferences := pref.new_preferences()
			preferences.backend = 'arm64'
			mut p := parser.Parser.new(preferences)
			mut a := p.parse_files([path, os.join_path(@VEXEROOT, 'vlib', 'strconv', 'structs.v')])
			assert p.diagnostics.len == 0, p.diagnostics.str()
			mut tc := types.TypeChecker.new(a)
			tc.building_v_fast = mode == 2
			tc.collect(a)
			if mode != 2 {
				tc.annotate_types()
			}
			assert tc.errors.len == 0, tc.errors.str()
			if mode == 1 {
				transform.transform(mut a, tc)
			} else if mode == 2 {
				_, _, errors := transform.transform_with_used_opt_config_scoped_workers_checked(mut a, tc, map[string]bool{}, false, true, false, true)
				assert errors.len == 0, errors.str()
			}
			m := ssa.build_with_used(a, map[string]bool{}, tc)
			mut g := Gen.new(m)
			g.gen()
			g.write_and_link(output)
			result := os.exec([output])
			assert result.exit_code == 0, 'mode=${mode}, exit ${result.exit_code}: ${result.output}'
		}
	}
}

fn test_native_decimal_parsing_preserves_float_constant_bits() {
	$if macos && arm64 {
		path := os.join_path(os.vtmp_dir(), 'arm64_float_parse_${os.getpid()}.v')
		output := path.all_before_last('.')
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
		}
		os.write_file(path, r'module main
import strconv
fn C.exit(int)
fn main() {
    cases := ["1.25", "1000.0", "-2.5", "0.00000125", "1e3", "-0.0", "inf", "-inf"]
    expected := [u64(0x3ff4000000000000), u64(0x408f400000000000), u64(0xc004000000000000),
        u64(0x3eb4f8b588e368f1), u64(0x408f400000000000), u64(0x8000000000000000),
        u64(0x7ff0000000000000), u64(0xfff0000000000000)]
    for index, text in cases {
        value := text.f64()
        bits := *unsafe { &u64(&value) }
        if bits != expected[index] { C.exit(index + 1) }
    }
    value32 := "1.25".f32()
    if *unsafe { &u32(&value32) } != u32(0x3fa00000) { C.exit(9) }
    exact := strconv.atof64("0.1", allow_extra_chars: false) or { C.exit(10) return }
    if *unsafe { &u64(&exact) } != u64(0x3fb999999999999a) { C.exit(11) }
    if "nan".f64() == "nan".f64() { C.exit(12) }
    literal := 1.25
    if *unsafe { &u64(&literal) } != u64(0x3ff4000000000000) { C.exit(13) }
    if "${literal / 1000.0:.5f}" != "0.00125" { C.exit(14) }
}
')!
		compiled := os.exec([@VEXE, '-gc', 'none', '-nocache', '-b', 'arm64', '-o', output, path])
		assert compiled.exit_code == 0, compiled.output
		result := os.exec([output])
		assert result.exit_code == 0, 'exit ${result.exit_code}: ${result.output}'
	}
}
