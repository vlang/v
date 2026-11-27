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
	mut args := if shared_v3 != '' { [shared_v3] } else { [@VEXE] }
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

fn test_string_indexes_use_checked_helpers_for_wide_integers() {
	generated := string_bounds_c('fn signed_wide(s string, i i128) u8 { return s[i] }
fn unsigned_wide(s string, i u128) u8 { return s[i] }
fn main() {
    _ = signed_wide("abc", 1)
    _ = unsigned_wide("abc", 1)
}
')
	assert generated.contains('return string__at_i128(s, i);'), generated
	assert generated.contains('return string__at_u128(s, i);'), generated
}

// The 0.5.2 compatibility compiler (launch_v1 fallback) checks the current
// vlib but has no i128/u128 types: its checker rejects any unguarded builtin
// declaration mentioning them. Files selected only with -d v3_backend stay
// hidden from it, so every other builtin source must avoid 128-bit types.
fn test_string_wide_index_helpers_stay_behind_v3_backend_guard() {
	builtin_dir := os.join_path(@VEXEROOT, 'vlib', 'builtin')
	files := os.ls(builtin_dir) or { panic(err) }
	mut offenders := []string{}
	for name in files {
		if !name.ends_with('.v') || name.ends_with('_test.v') {
			continue
		}
		if name.all_before('.v').contains('_d_') {
			continue
		}
		text := os.read_file(os.join_path(builtin_dir, name)) or { panic(err) }
		if mentions_128bit_type(text) {
			offenders << name
		}
	}
	assert offenders.len == 0, 'builtin sources visible to the compat compiler mention 128-bit types: ${offenders}'
	guarded := os.read_file(os.join_path(builtin_dir, 'string_d_v3_backend.v')) or {
		panic(err)
	}
	assert guarded.contains('fn (s string) at_i128(idx i128) u8')
	assert guarded.contains('fn (s string) at_u128(idx u128) u8')
}

fn mentions_128bit_type(text string) bool {
	for token in ['i128', 'u128'] {
		mut start := 0
		for {
			idx := text.index_after(token, start) or { break }
			before_ok := idx == 0 || (!text[idx - 1].is_alnum() && text[idx - 1] != `_`)
			after_idx := idx + token.len
			after_ok := after_idx >= text.len
				|| (!text[after_idx].is_alnum() && text[after_idx] != `_`)
			if before_ok && after_ok {
				return true
			}
			start = idx + 1
		}
	}
	return false
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
