module types

import os
import v.parser
import v.pref

struct IntegerCastDiagnostics {
	errors   []TypeError
	warnings []TypeError
}

fn check_integer_cast_platform_source(source string) IntegerCastDiagnostics {
	path := os.join_path(os.vtmp_dir(), 'integer_cast_platform_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.diagnostic_files[path] = true
	tc.check_semantics()
	return IntegerCastDiagnostics{
		errors:   tc.errors.clone()
		warnings: tc.notices.filter(it.severity == 'warning:')
	}
}

fn test_int_literal_casts_use_the_configured_target_width() {
	saved_bits := platform_int_bits()
	defer {
		set_platform_int_bits(saved_bits)
	}
	for bits in [32, 64] {
		set_platform_int_bits(bits)
		valid := if bits == 64 {
			['2147483648', '4294967296', '9223372036854775807', '-9223372036854775808',
				'0x7fff_ffff_ffff_ffff']
		} else {
			['2147483647', '-2147483648', '0x7fff_ffff']
		}
		for literal in valid {
			result := check_integer_cast_platform_source('fn main() { _ = int(${literal}) }')
			assert result.errors.len == 0, '${bits}: ${literal}: ${result.errors}'
			assert result.warnings.len == 0, '${bits}: ${literal}: ${result.warnings}'
		}
		alias := check_integer_cast_platform_source('type Count = int
fn main() { _ = Count(${valid[0]}) }')
		assert alias.errors.len == 0, alias.errors.str()
		assert alias.warnings.len == 0, alias.warnings.str()
	}
}

fn test_int_literal_casts_keep_target_and_fixed_width_overflow_checks() {
	saved_bits := platform_int_bits()
	defer {
		set_platform_int_bits(saved_bits)
	}
	set_platform_int_bits(32)
	too_wide := check_integer_cast_platform_source('fn main() { _ = int(4294967296) }')
	assert too_wide.errors.len == 1, too_wide.errors.str()
	assert too_wide.errors[0].msg == 'value `4294967296` overflows `int`'
	set_platform_int_bits(64)
	fixed := check_integer_cast_platform_source('fn main() { _ = i32(4294967296) }')
	assert fixed.errors.len == 1, fixed.errors.str()
	assert fixed.errors[0].msg == 'value `4294967296` overflows `i32`'
	beyond_u64 := check_integer_cast_platform_source('fn main() { _ = int(18446744073709551616) }')
	assert beyond_u64.errors.any(it.msg == 'value `18446744073709551616` overflows `int`')
}

fn test_int_literal_cast_overflow_warnings_follow_the_target_width() {
	saved_bits := platform_int_bits()
	defer {
		set_platform_int_bits(saved_bits)
	}
	for bits in [32, 64] {
		set_platform_int_bits(bits)
		literals := if bits == 64 {
			['0xffffffffffffffff', '9223372036854775808', '-9223372036854775809', '-0xffffffffffffffff']
		} else {
			['0xffffffff', '2147483648', '-2147483649', '-0xffffffff']
		}
		for literal in literals {
			result := check_integer_cast_platform_source('fn main() { _ = int(${literal}) }')
			assert result.errors.len == 0, '${bits}: ${literal}: ${result.errors}'
			assert result.warnings.len == 1, '${bits}: ${literal}: ${result.warnings}'
			assert result.warnings[0].msg == 'value `${literal}` overflows `int`, this will be considered hard error soon'
		}
	}
}
