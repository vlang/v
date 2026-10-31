module c

import os
import v.markused
import v.parser
import v.pref
import v.transform
import v.types

fn string_bounds_c(source string) string {
	path := os.join_path(os.vtmp_dir(), 'string_bounds_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	transform.transform(mut a, &tc)
	tc.annotate_types()
	used := markused.mark_used(a, tc)
	mut g := FlatGen.new()
	return g.gen_with_used_options(a, used, &tc, true)
}

fn test_string_indexes_use_checked_helpers_without_narrowing() {
	generated := string_bounds_c('type Wide = u64
fn signed(s string, i int) u8 { return s[i] }
fn signed_wide(s string, i i64) u8 { return s[i] }
fn unsigned_wide(s string, i Wide) u8 { return s[i] }
fn direct(s string, i int) u8 { return unsafe { s[i] } }
fn main() {
    _ = signed("abc", 1)
    _ = signed_wide("abc", 1)
    _ = unsigned_wide("abc", Wide(1))
    _ = direct("abc", 1)
}
')
	assert generated.contains('return string__at(s, i);'), generated
	assert generated.contains('return string__at_i64(s, i);'), generated
	assert generated.contains('return string__at_u64(s, i);'), generated
	assert generated.contains('return (s).str[i];'), generated
}

fn test_string_indexes_panic_at_invalid_signed_and_unsigned_positions() {
	root := os.join_path(os.vtmp_dir(), 'string_bounds_runtime_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for index in ['int(3)', 'int(-1)', 'i64(4294967296)', 'u64(18446744073709551615)'] {
		source := os.join_path(root, 'main.v')
		os.write_file(source, 'fn main() { s := "abc"; println(s[${index}]); println("no panic") }')!
		result := os.exec([@VEXE, '-cc', 'clang', '-gc', 'none', '-no-retry-compilation', 'run',
			source])
		assert result.exit_code != 0, result.output
		assert result.output.contains('string index out of range'), result.output
		assert !result.output.contains('no panic'), result.output
	}
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() { s := "abc"; i := 3; assert s[i] == 0 }')!
	result := os.exec([@VEXE, '-cc', 'clang', '-gc', 'none', '-no-bounds-checking',
		'-no-retry-compilation', 'run', source])
	assert result.exit_code == 0, result.output
}
