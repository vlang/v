module main

import os

fn test_string_indexes_use_checked_helpers_without_narrowing() {
	root := os.join_path(os.vtmp_dir(), 'string_bounds_codegen_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'input.v')
	c_path := os.join_path(root, 'output.c')
	os.write_file(path, 'type Wide = u64
fn signed_index(s string, i int) u8 { return s[i] }
fn signed_wide(s string, i i64) u8 { return s[i] }
fn unsigned_wide(s string, i Wide) u8 { return s[i] }
fn direct(s string, i int) u8 { return unsafe { s[i] } }
fn main() {
    _ = signed_index("abc", 1)
    _ = signed_wide("abc", 1)
    _ = unsigned_wide("abc", Wide(1))
    _ = direct("abc", 1)
}
')!
	// V1-hosted unit tests get a shared V3 compiler from their runner.
	shared_v3 := os.getenv('V3_TEST_UNIT_SHARED_V3')
	mut args := if shared_v3 != '' { [shared_v3] } else { [@VEXE, '-new-compiler'] }
	args << ['-gc', 'none', '-o', c_path, path]
	compiled := os.exec(args)
	assert compiled.exit_code == 0, compiled.output
	generated := os.read_file(c_path)!
	for name, expected in {
		'signed_index':  'return string__at(s, i);'
		'signed_wide':   'return string__at_i64(s, i);'
		'unsigned_wide': 'return string__at_u64(s, i);'
		'direct':        'return (s).str[i];'
	} {
		body := generated.all_after_last('u8 ${name}(').all_after('{').all_before('\n}')
		assert body.contains(expected), '${name}: ${body}'
	}
}

fn test_string_indexes_panic_at_invalid_signed_and_unsigned_positions() {
	root := os.join_path(os.vtmp_dir(), 'string_bounds_runtime_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	binary := os.join_path(root, if os.user_os() == 'windows' { 'main.exe' } else { 'main' })
	os.write_file(source, '
import os

fn main() {
	s := "abc"
	match os.args[1] {
		"past_end" { println(s[int(3)]) }
		"negative" { println(s[int(-1)]) }
		"wide_signed" { println(s[i64(4294967296)]) }
		"wide_unsigned" { println(s[u64(18446744073709551615)]) }
		else { exit(2) }
	}
	println("no panic")
}
')!
	// A Windows `clang` usually targets the MSVC ABI, which V cannot build Windows
	// programs with, so use the MinGW gcc there.
	cc := $if windows { 'gcc' } $else { 'clang' }
	compiled := os.exec([@VEXE, '-cc', cc, '-gc', 'none', '-no-retry-compilation', '-o', binary,
		source])
	assert compiled.exit_code == 0, compiled.output
	for case_name in ['past_end', 'negative', 'wide_signed', 'wide_unsigned'] {
		result := os.exec([binary, case_name])
		assert result.exit_code != 0, '${case_name}: ${result.output}'
		assert result.output.contains('string index out of range'), '${case_name}: ${result.output}'
		assert !result.output.contains('no panic'), '${case_name}: ${result.output}'
	}

	os.write_file(source, 'fn main() { s := "abc"; i := 3; assert s[i] == 0 }')!
	result := os.exec([@VEXE, '-cc', cc, '-gc', 'none', '-no-bounds-checking', '-no-retry-compilation',
		'run', source])
	assert result.exit_code == 0, result.output
}
