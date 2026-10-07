module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_rune_writes_preserve_utf8_lengths_pointer_slots_and_builder_growth() {
	$if macos && arm64 {
		utf8 := os.read_file(os.join_path(@VMODROOT, 'vlib', 'builtin', 'utf8.v')) or {
			panic(err)
		}
		strings := os.read_file(os.join_path(@VMODROOT, 'vlib', 'strings', 'builder.c.v')) or {
			panic(err)
		}
		string_source := os.read_file(os.join_path(@VMODROOT, 'vlib', 'builtin', 'string.v')) or {
			panic(err)
		}
		conversion := utf8.all_after('pub fn utf32_to_str_no_malloc').all_before('// Convert utf8 to utf32')
		writer := strings.all_after('pub fn (mut b Builder) write_rune').all_before('// write_runes')
		tos := string_source.all_after('pub fn tos(').all_before('// tos2')
		string_decl := string_source.all_after('pub struct string {').all_before('// runes returns')
		builtin_source := 'module builtin
fn C.exit(int)
pub struct string {' + string_decl + '
pub fn array_new(element_size i64, length i64, capacity i64) []u8 { return []u8{} }
pub fn (mut a array) push_many(data voidptr, count int) {}
@[unsafe]
pub fn tos(' + tos + '\n@[manualfree; unsafe]\npub fn utf32_to_str_no_malloc' + conversion +
			'\n'
		strings_source := 'module strings
pub type Builder = []u8
pub fn new_builder(capacity int) Builder { return Builder(array_new(1, 0, i64(capacity))) }
pub fn (mut b Builder) str() string { return "" }
@[manualfree]
pub fn (mut b Builder) write_rune' + writer
		source := r'module main
import strings
fn main() {
    mut buffer := [5]u8{}
    first := unsafe { utf32_to_str_no_malloc(65, mut &buffer[0]) }
    if first.len != 1 || first != "A" || buffer[0] != 65 || buffer[1] != 0 { C.exit(1) }
    second := unsafe { utf32_to_str_no_malloc(233, mut &buffer[0]) }
    if second.len != 2 || second != "é" || buffer[2] != 0 { C.exit(2) }
    third := unsafe { utf32_to_str_no_malloc(8364, mut &buffer[0]) }
    if third.len != 3 || third != "€" || buffer[3] != 0 { C.exit(3) }
    fourth := unsafe { utf32_to_str_no_malloc(128578, mut &buffer[0]) }
    if fourth.len != 4 || fourth != "🙂" || buffer[4] != 0 { C.exit(4) }
    mut out := strings.new_builder(1)
    out.write_rune(rune(65))
    out.write_rune(rune(233))
    out.write_rune(rune(8364))
    out.write_rune(rune(128578))
    out.write_rune(rune(0))
    out.write_rune(rune(1114112))
    result := out.str()
    if result.len != 11 || result != "Aé€🙂\0" { C.exit(5) }
}
'
		path := os.join_path(os.vtmp_dir(), 'arm64_rune_${os.getpid()}.v')
		output := path.all_before_last('.')
		builtin_path := output + '_builtin.v'
		strings_path := output + '_strings.v'
		defer {
			os.rm(path) or {}
			os.rm(builtin_path) or {}
			os.rm(strings_path) or {}
			os.rm(output) or {}
		}
		os.write_file(path, source) or { panic(err) }
		os.write_file(builtin_path, builtin_source) or { panic(err) }
		os.write_file(strings_path, strings_source) or { panic(err) }
		mut p := parser.Parser.new(pref.new_preferences())
		mut a := p.parse_files([builtin_path, strings_path, path])
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := types.TypeChecker.new(a)
		tc.collect(a)
		tc.annotate_types()
		assert tc.errors.len == 0, tc.errors.str()
		transform.transform(mut a, tc)
		m := ssa.build_with_used(a, map[string]bool{}, tc)
		mut g := Gen.new(m)
		g.gen()
		g.write_and_link(output)
		result := os.exec([output])
		assert result.exit_code == 0, 'exit ${result.exit_code}: ${result.output}'
	}
}
