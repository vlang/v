module driver

import os
import v.cmdexec

fn test_input_is_compiler_tree() {
	assert input_is_compiler_tree('${@VEXEROOT}/vlib/v')
	assert input_is_compiler_tree('${@VEXEROOT}/vlib/v/transform/fn_test.v')
	assert input_is_compiler_tree('${@VEXEROOT}/vlib/v/compiler_tests/driver_cli_test.v')
	assert !input_is_compiler_tree('${@VEXEROOT}/cmd/v')
	assert !input_is_compiler_tree('${@VEXEROOT}/vlib/v/tests/array_test.v')
	assert !input_is_compiler_tree('${@VEXEROOT}/vlib/v/parser/tests/invalid_syntax.vv')
	assert !input_is_compiler_tree('${@VEXEROOT}/vlib/v/slow_tests/inout/compiler_test.v')
}

fn test_memory_limit_units_and_range() {
	for unit in ['K', 'k'] {
		assert parse_memory_limit('128' + unit)! == 128
	}
	for unit in ['', 'M', 'm'] {
		assert parse_memory_limit('128' + unit)! == 128 * 1024
	}
	for unit in ['G', 'g'] {
		assert parse_memory_limit('128' + unit)! == 128 * 1024 * 1024
	}
	assert parse_memory_limit('0')! == 0
	assert parse_memory_limit('0G')! == 0
	assert parse_memory_limit('9223372036854775807K')! == max_i64
	assert parse_memory_limit('9007199254740991M')! == max_i64 - 1023
	assert parse_memory_limit('8796093022207G')! == max_i64 - 1048575
	for value in ['', 'K', 'nonsense', '-1', '1.5M', '1T', '1 K', '9223372036854775808',
		'9007199254740992M', '8796093022208G', '17592186044416G', '9223372036854775807G'] {
		if limit := parse_memory_limit(value) {
			assert false, '${value} unexpectedly parsed as ${limit}'
		}
	}
}

fn test_memory_limit_cli_validation_and_guard() {
	root := os.join_path(os.vtmp_dir(), 'memory_limit_cli_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main.c')
	os.write_file(source, 'fn main() {}\n')!
	for option in ['-memory-limit', '--memory-limit'] {
		missing := cmdexec.run(@VEXE, ['-new-compiler', option])
		assert missing.exit_code != 0, missing.output
		assert missing.output.contains('requires a value'), missing.output
		for value in ['', 'nonsense', '-1', '17592186044416G', '9223372036854775807G'] {
			invalid := cmdexec.run(@VEXE, ['-new-compiler', option, value, '-o', output, source])
			assert invalid.exit_code != 0, invalid.output
			assert !invalid.output.contains('V panic'), invalid.output
			assert invalid.output.contains('requires a value')
				|| invalid.output.contains('invalid value'), invalid.output
		}
		for value in ['1', '1M'] {
			limited := cmdexec.run(@VEXE, ['-new-compiler', '-silent', option, value, '-o', output,
				source])
			assert limited.exit_code != 0, limited.output
			assert limited.output.contains('limit: 1 MiB'), limited.output
		}
		for value in ['0', '0K'] {
			unlimited := cmdexec.run(@VEXE, ['-new-compiler', '-silent', option, value, '-o', output,
				source])
			assert unlimited.exit_code == 0, unlimited.output
		}
		formatted := cmdexec.run(@VEXE, ['-new-compiler', option, '1G', 'fmt', '-verify', source])
		assert formatted.exit_code == 0, formatted.output
	}
}
