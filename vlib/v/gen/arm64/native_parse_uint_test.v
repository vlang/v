module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_unsigned_literals_preserve_real_strconv_overflow_checks() {
	$if macos && arm64 {
		integers := os.read_file(os.join_path(@VMODROOT, 'vlib', 'builtin', 'int.v'))!
		atoi := os.read_file(os.join_path(@VMODROOT, 'vlib', 'strconv', 'atoi.v'))!
		sources := [
			'module builtin\npub const min_i8' + integers.all_after('pub const min_i8').all_before('// str_l returns'),
			'module strconv\nconst int_size = 32\npub fn common_parse_uint2' +
				atoi.all_after('pub fn common_parse_uint2').all_before('// parse_uint is like'),
			r'module main
import strconv
fn C.exit(int)
fn main() {
    value, error_code := strconv.common_parse_uint2("42", 10, 64)
    if value != u64(42) || error_code != 0 { C.exit(1) }
    small, small_error := strconv.common_parse_uint2("97", 10, 32)
    if small != u64(97) || small_error != 0 { C.exit(2) }
    maximum32, maximum32_error := strconv.common_parse_uint2("4294967295", 10, 32)
    if maximum32 != u64(max_u32) || maximum32_error != 0 { C.exit(3) }
    overflow32, overflow32_error := strconv.common_parse_uint2("4294967296", 10, 32)
    if overflow32 != u64(max_u32) || overflow32_error != -3 { C.exit(4) }
    maximum64, maximum64_error := strconv.common_parse_uint2("18446744073709551615", 10, 64)
    if maximum64 != max_u64 || maximum64_error != 0 { C.exit(5) }
    overflow64, overflow64_error := strconv.common_parse_uint2("18446744073709551616", 10, 64)
    if overflow64 != max_u64 || overflow64_error != -3 { C.exit(6) }
    hexadecimal, hexadecimal_error := strconv.common_parse_uint2("0xffff_ffff", 0, 32)
    if hexadecimal != u64(max_u32) || hexadecimal_error != 0 { C.exit(7) }
    boundary, boundary_error := strconv.common_parse_uint2("9223372036854775808", 10, 64)
    if boundary != u64(1) << 63 || boundary_error != 0 { C.exit(8) }
}
',
		]
		mut paths := []string{}
		for index, source in sources {
			path := os.join_path(os.vtmp_dir(), 'arm64_parse_uint_${os.getpid()}_${index}.v')
			os.write_file(path, source)!
			paths << path
		}
		output := paths[0].all_before_last('.')
		defer {
			for path in paths {
				os.rm(path) or {}
			}
			os.rm(output) or {}
		}
		mut preferences := pref.new_preferences()
		preferences.backend = 'arm64'
		mut p := parser.Parser.new(preferences)
		mut a := p.parse_files(paths)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := types.TypeChecker.new(a)
		tc.collect(a)
		tc.annotate_types()
		assert tc.errors.len == 0, tc.errors.str()
		transform.transform(mut a, tc)
		m := ssa.build_with_used(a, map[string]bool{}, tc)
		mut unsigned_division := false
		for f in m.funcs {
			if f.name == 'strconv.common_parse_uint2' {
				for block in f.blocks {
					for value in m.blocks[block].instrs {
						if m.instrs[m.values[value].index].op == .udiv {
							unsigned_division = true
						}
					}
				}
			}
		}
		assert unsigned_division
		mut g := Gen.new(m)
		g.gen()
		g.write_and_link(output)
		result := os.exec([output])
		assert result.exit_code == 0, 'exit ${result.exit_code}: ${result.output}'
	}
}
