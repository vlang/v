module main

import os

fn test_translated_assert_accepts_c_scalar_conditions() ! {
	root := os.join_path(os.vtmp_dir(), 'translated_assert_pass_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, '@[translated]
module main
type Number = int
type Callback = fn ()
enum Flag { zero one }
fn callback() {}
fn next(mut calls []int) int { calls[0]++; return calls[0] * 256 }
fn main() {
	value := 2
	assert if value >= 1 && value <= 3 { true } else { false }
	assert if value == 0 { 0 } else { 256 }
	assert if value == 0 { 0.0 } else { 0.5 }
	assert Number(-1)
	assert Flag.one
	assert i64(-256)
	assert u64(256)
	assert f64(-0.5)
	assert char(1)
	assert &value
	pointer := unsafe { &int(voidptr(256)) }
	null := unsafe { &int(nil) }
	assert if value { pointer } else { null }
	assert match value { 2 { pointer } else { null } }
	cb := Callback(callback)
	assert cb
	null_callback := unsafe { Callback(nil) }
	assert if value { cb } else { null_callback }
	assert match value { 2 { cb } else { null_callback } }
	mut calls := [0]
	assert next(mut calls)
	assert calls[0] == 1
	assert if value { next(mut calls) } else { 0 }
	assert calls[0] == 2
	assert match value { 2 { next(mut calls) } else { 0 } }
	assert calls[0] == 3
	println("passed")
}
')!
	mut variations := [[]string{}, ['-no-parallel']]
	$if !windows {
		// MSVC uses one-byte integer bool storage, without _Bool's conversion.
		variations << ['-cflags', '-D_Bool=char']
	}
	for flags in variations {
		result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-gc', 'none', '-cc',
			@CCOMPILER, ...flags, 'run', source])
		assert result.exit_code == 0, result.output
		assert result.output.trim_space() == 'passed', result.output
	}
}

fn test_translated_assert_rejects_zero_conditions_at_runtime() ! {
	root := os.join_path(os.vtmp_dir(), 'translated_assert_fail_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	program := os.join_path(root, 'assert_condition' + $if windows { '.exe' } $else { '' })
	os.write_file(source, '@[translated]
module main
import os
type Callback = fn ()
enum Flag { zero one }
fn main() {
	match os.args[1] {
		"if" { value := 0; assert if value { true } else { false }, "translated condition failed" }
		"integer" { assert 0, "translated condition failed" }
		"float" { assert f64(0), "translated condition failed" }
		"enum" { assert Flag.zero, "translated condition failed" }
		"pointer" { pointer := unsafe { &int(nil) }; assert pointer, "translated condition failed" }
		"callback" { cb := unsafe { Callback(nil) }; assert cb, "translated condition failed" }
		"pointer_if" { pointer := unsafe { &int(nil) }; assert if true { pointer } else { pointer }, "translated condition failed" }
		"callback_if" { cb := unsafe { Callback(nil) }; assert if true { cb } else { cb }, "translated condition failed" }
		"pointer_match" { pointer := unsafe { &int(nil) }; assert match 1 { 1 { pointer } else { pointer } }, "translated condition failed" }
		"callback_match" { cb := unsafe { Callback(nil) }; assert match 1 { 1 { cb } else { cb } }, "translated condition failed" }
		else { panic("unknown case") }
	}
}
')!
	for flags in [[]string{}, ['-no-parallel']] {
		built := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-gc', 'none', '-cc',
			@CCOMPILER, ...flags, '-o', program, source])
		assert built.exit_code == 0, built.output
		for condition in ['if', 'integer', 'float', 'enum', 'pointer', 'callback', 'pointer_if',
			'callback_if', 'pointer_match', 'callback_match'] {
			result := os.exec([program, condition])
			assert result.exit_code != 0, '${condition}: ${result.output}'
			assert result.output.contains('translated condition failed'), '${condition}: ${result.output}'
		}
	}
}

fn test_translated_assert_supports_portable_int128_conditions() ! {
	root := os.join_path(os.vtmp_dir(), 'translated_assert_int128_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	program := os.join_path(root, 'assert_condition' + $if windows { '.exe' } $else { '' })
	os.write_file(source, '@[translated]
module main
import os
fn main() {
	match os.args[1] {
		"pass" {
			wide := u128(1) << 64
			assert wide
			assert i128(-1)
			assert if true { wide } else { u128(0) }
			assert match 1 { 1 { wide } else { u128(0) } }
			println("passed")
		}
		"unsigned_zero" { assert u128(0), "translated wide condition failed" }
		"signed_zero" { assert i128(0), "translated wide condition failed" }
		"if_zero" { assert if true { u128(0) } else { u128(1) }, "translated wide condition failed" }
		else { panic("unknown case") }
	}
}
')!
	for flags in [[]string{}, ['-no-parallel']] {
		built := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-gc', 'none', '-cc',
			@CCOMPILER, '-d', 'v3_no_native_int128', ...flags, '-o', program, source])
		assert built.exit_code == 0, built.output
		passed := os.exec([program, 'pass'])
		assert passed.exit_code == 0, passed.output
		assert passed.output.trim_space() == 'passed', passed.output
		for condition in ['unsigned_zero', 'signed_zero', 'if_zero'] {
			result := os.exec([program, condition])
			assert result.exit_code != 0, '${condition}: ${result.output}'
			assert result.output.contains('translated wide condition failed'), '${condition}: ${result.output}'
		}
	}
}

fn test_translated_assert_rules_preserve_invalid_condition_checks() ! {
	root := os.join_path(os.vtmp_dir(), 'translated_assert_checks_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'translated.v'), '@[translated]\nmodule main\nfn translated_marker() {}\n')!
	source := os.join_path(root, 'main.v')
	declarations := 'module main
enum Flag { zero one }
struct Record { value int }
fn callback() {}
fn optional_number() ?int { return 1 }
fn optional_flag() ?bool { return true }
'
	ordinary_cases := ['assert 1', 'assert 0.5', 'assert Flag.one', 'value := 1; assert &value',
		'assert callback']
	translated_cases := ['assert "text"', 'assert [1]', 'assert [1]!', 'assert {"key": 1}',
		'assert Record{value: 1}', 'assert optional_number()', 'assert optional_flag()']
	for translated in [false, true] {
		prefix := if translated { '@[translated]\n' } else { '' }
		cases := if translated { translated_cases } else { ordinary_cases }
		for body in cases {
			os.write_file(source, prefix + declarations + 'fn main() { translated_marker(); ${body} }\n')!
			for flags in [[]string{}, ['-no-parallel']] {
				result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', ...flags, '-check',
					root])
				assert result.exit_code != 0, '${body}: ${result.output}'
				assert result.output.contains('assert can be used only with `bool` expressions'), '${body}: ${result.output}'
			}
		}
	}
	os.write_file(source, '@[translated]\nmodule main\nfn main() { assert 1, 42 }\n')!
	message := os.exec([@VEXE, '-new-compiler', '-check', root])
	assert message.exit_code != 0, message.output
	assert message.output.contains('assert allows only a single string'), message.output
}
