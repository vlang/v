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

fn test_native_float_formatting_matches_c_rounding_and_shortest_decimal_expansion() {
	$if macos && arm64 {
		path := os.join_path(os.vtmp_dir(), 'arm64_float_format_parity_${os.getpid()}.v')
		c_output := path.all_before_last('.') + '_c'
		native_output := path.all_before_last('.') + '_native'
		defer {
			os.rm(path) or {}
			os.rm(c_output) or {}
			os.rm(native_output) or {}
		}
		mut source := 'module main\nimport strconv\nimport math\nfn main() {\n'
		mut cases := []string{}
		precisions := [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
			35, 40, 200]
		values := ['1.25', '-1.25', '1.125', '-1.125', '2.15', '-2.15', '2.675', '0.125', '0.005',
			'-0.005', '0.0005', '9.95', '-9.95', '9.995', '-9.995', '99.95', '999.5', '0.1',
			'1.2345678901234567', '1e23', '-1e23', '0.0', 'math.copysign(0.0, -1.0)', 'math.inf(1)',
			'math.inf(-1)', 'math.nan()']
		for typ in ['f64', 'f32'] {
			for value in values {
				for precision in precisions {
					expression := '${typ}(${value})'
					format := '${expression}:.${precision}f'
					source += "println('" + r'${' + format + "}')\n"
					cases << format
				}
			}
			for value in ['1e23', '-1e23', '1.234e23', '-1.234e23', '1e20', '1e-6', '1e-7', '1.234e-7',
				'1e-20', '0.0', 'math.copysign(0.0, -1.0)', 'math.inf(1)', 'math.inf(-1)', 'math.nan()',
				if typ == 'f32' { 'math.max_f32' } else { 'math.max_f64' }, if typ == 'f32' {
					'math.smallest_non_zero_f32'
				} else {
					'math.smallest_non_zero_f64'
				}] {
				for suffix in ['', '_with_dot'] {
					expression := 'strconv.${typ}_to_str_l${suffix}(${typ}(${value}))'
					source += 'println(${expression})\n'
					cases << expression
				}
			}
		}
		for value in ['0.0', 'math.copysign(0.0, -1.0)', 'math.inf(1)', 'math.inf(-1)', 'math.nan()'] {
			source += "println('" + r'${' + value + ":.40}')\n"
			cases << '${value}:.40'
		}
		source += '}\n'
		os.write_file(path, source)!
		for backend, output in {
			'c':     c_output
			'arm64': native_output
		} {
			compiled := os.exec(['env', 'VFLAGS=', 'V_MACOS_V3_NO_FALLBACK=1', @VEXE, '-gc', 'none',
				'-nocache', '-cc', 'clang', '-b', backend, '-o', output, path])
			assert compiled.exit_code == 0, compiled.output
		}
		reference := os.exec([c_output])
		assert reference.exit_code == 0, reference.output
		native := os.exec([native_output])
		assert native.exit_code == 0, native.output
		expected_lines := reference.output.split_into_lines()
		actual_lines := native.output.split_into_lines()
		assert actual_lines.len == cases.len, native.output
		assert expected_lines.len == cases.len, reference.output
		for index, expected in expected_lines {
			assert actual_lines[index] == expected, '${cases[index]}: native=${actual_lines[index]}, C=${expected}'
		}
	}
}

fn test_native_float_unary_minus_preserves_zero_sign_bits() {
	$if macos && arm64 {
		path := os.join_path(os.vtmp_dir(), 'arm64_negative_zero_${os.getpid()}.v')
		output := path.all_before_last('.')
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
		}
		os.write_file(path, 'module main
fn C.exit(int)
union Bits64 { f f64 u u64 }
union Bits32 { f f32 u u32 }
fn literal64() f64 { return -0.0 }
fn literal32() f32 { return -f32(0.0) }
fn negate64(value f64) f64 { return -value }
fn negate32(value f32) f32 { return -value }
fn main() {
    literal_64 := Bits64{f: literal64()}
    literal_32 := Bits32{f: literal32()}
    negated_64 := Bits64{f: negate64(0.0)}
    negated_32 := Bits32{f: negate32(f32(0.0))}
    positive_64 := Bits64{f: negate64(literal64())}
    positive_32 := Bits32{f: negate32(literal32())}
    if unsafe { literal_64.u } != u64(0x8000000000000000) { C.exit(1) }
    if unsafe { literal_32.u } != u32(0x80000000) { C.exit(2) }
    if unsafe { negated_64.u } != u64(0x8000000000000000) { C.exit(3) }
    if unsafe { negated_32.u } != u32(0x80000000) { C.exit(4) }
    if unsafe { positive_64.u } != u64(0) { C.exit(5) }
    if unsafe { positive_32.u } != u32(0) { C.exit(6) }
    if negate64(17.0) != -17.0 || negate32(f32(-17.0)) != f32(17.0) { C.exit(7) }
}
')!
		for production in [false, true] {
			mut args := ['env', 'VFLAGS=', 'V_MACOS_V3_NO_FALLBACK=1', @VEXE, '-new-compiler',
				'-no-retry-compilation', '-cc', 'clang', '-gc', 'none', '-nocache', '-b', 'arm64',
				'-o', output, path]
			if production { args.insert(5, '-prod') }
			compiled := os.exec(args)
			assert compiled.exit_code == 0, compiled.output
			result := os.exec([output])
			assert result.exit_code == 0, 'production=${production}, exit ${result.exit_code}: ${result.output}'
		}
	}
}
