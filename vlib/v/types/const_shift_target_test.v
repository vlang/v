module types

import os
import v.parser
import v.pref

fn const_shift_target_check(source string) ([]TypeError, []TypeError) {
	path := os.join_path(os.vtmp_dir(), 'const_shift_target_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.filter(it.severity == 'error:').len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.diagnostic_files[path] = true
	tc.check_semantics()
	return tc.errors.clone(), tc.notices.clone()
}

fn test_constant_shift_counts_preserve_platform_unsigned_wrapping() {
	saved_bits := platform_int_bits()
	defer { set_platform_int_bits(saved_bits) }
	for bits in [32, 64] {
		set_platform_int_bits(bits)
		for typ in ['uint', 'usize'] {
			errors, _ := const_shift_target_check('type Count = ${typ}
const count = Count(4294967295) + 1
fn main() { _ = u64(1) << count }')
			assert errors.len == if bits == 32 { 0 } else { 1 }, '${bits}: ${errors}'
			if bits == 64 {
				assert errors[0].msg == 'shift count for type `u64` too large (maximum: 63 bits)'
			}
		}
	}
}

fn test_const_shift_generic_work_survives_deep_and_repeated_calls() {
	mut source := ''
	for name in ['deep', 'repeated'] {
		source += 'fn ${name}0[T]() T { return T(1) << 8 }\n'
		for depth in 1 .. 41 {
			source += 'fn ${name}${depth}[T]() T { return ${name}${depth - 1}[T]() }\n'
		}
	}
	source += 'fn main() { _ = deep40[u8]()\n _ = repeated40[u8]()\n _ = repeated2[u8]() }'
	errors, notices := const_shift_target_check(source)
	assert errors.filter(it.msg.starts_with('shift count for type ')).len == 2, errors.str()
	assert notices.len == 0, notices.str()
}

fn test_const_shift_generic_chain_without_candidate_operations() {
	mut source := 'fn level0[T]() T { return T(1) }\n'
	for depth in 1 .. 601 {
		source += 'fn level${depth}[T]() T { return level${depth - 1}[T]() }\n'
	}
	source += 'fn main() {\n'
	for depth in 0 .. 601 {
		source += '_ = level${depth}[u8]()\n'
	}
	source += '}\n'
	errors, notices := const_shift_target_check(source)
	assert errors.len == 0, errors.str()
	assert notices.len == 0, notices.str()
}
