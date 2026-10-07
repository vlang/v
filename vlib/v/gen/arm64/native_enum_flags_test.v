module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_enum_flag_methods_preserve_array_alias_headers_and_mutable_storage() {
	$if macos && arm64 {
		array_source := os.read_file(os.join_path(@VMODROOT, 'vlib', 'builtin', 'array.v'))!
		builder_source := os.read_file(os.join_path(@VMODROOT, 'vlib', 'strings', 'builder.c.v'))!
		reuse_source := 'pub fn (mut b Builder) reuse_as_plain_u8_array() []u8 {' +
			builder_source.all_after('pub fn (mut b Builder) reuse_as_plain_u8_array() []u8 {').all_before('\n}') +
			'\n}\n'
		sources := [
			array_source.all_before('// Bit 31') + '\npub fn array_new(element_size i64, length i64, capacity i64) []u8 { return []u8{} }\n',
			r'module strings
pub type Builder = []u8
pub fn new_builder(size int) Builder {
    return Builder(array_new(1, 0, i64(size)))
}
pub fn (mut b Builder) write_string(value string) {}
' + reuse_source,
			r'module unrelated
fn C.exit(int)
struct VarTypeIndexCache { mut: value string }
fn (mut cache VarTypeIndexCache) clear() { C.exit(90) }
',
			r'module main
import strings
fn C.exit(int)
fn C.memcpy(voidptr, voidptr, usize) voidptr
@[flag]
enum Features { read write execute extra }
type FeatureAlias = Features
struct Container {
mut:
    prefix u64 = 123456789
    flags FeatureAlias
    suffix u64 = 987654321
}
struct NativeArray {
    data voidptr
    offset i32
    len i32
    cap i32
    flags u32
    element_size i32
}
fn mutate_flags(mut flags FeatureAlias) {
    flags.set(.write)
    flags.clear(.execute)
}
fn main() {
    mut container := Container{flags: FeatureAlias(Features.read | .execute)}
    mutate_flags(mut container.flags)
    if !container.flags.has(.read) { C.exit(12) }
    if !container.flags.all(.read | .write) { C.exit(13) }
    if container.flags.has(.execute) { C.exit(14) }
    if !container.flags.has(.read | .execute) || container.flags.all(.read | .execute) { C.exit(2) }
    mask := Features.write | .extra
    container.flags.set(mask)
    if !container.flags.all(mask) { C.exit(3) }
    mut pointer := &container.flags
    pointer.clear(mask)
    if container.flags.has(mask) { C.exit(15) }
    if pointer.has(mask) { C.exit(4) }
    if !pointer.has(.read) { C.exit(16) }
    if pointer.has(Features(0)) || !pointer.all(Features(0)) { C.exit(5) }
    if container.prefix != 123456789 || container.suffix != 987654321 { C.exit(6) }
    mut builder := strings.new_builder(4)
    builder.write_string("native flags")
    unsafe { builder.flags.set(.noslices) }
    mut before := NativeArray{}
    C.memcpy(&before, &builder, sizeof(NativeArray))
    mut bytes := unsafe { builder.reuse_as_plain_u8_array() }
    mut after := NativeArray{}
    C.memcpy(&after, &bytes, sizeof(NativeArray))
    if before.flags & 1 != 1 || after.flags & 1 != 0 { C.exit(7) }
    if after.flags != before.flags & ~u32(1) || after.data != before.data
        || after.len != before.len || after.cap != before.cap
        || after.element_size != 1 { C.exit(8) }
    if bytes.len != 12 || bytes[0] != u8(110) || bytes[11] != u8(115) { C.exit(9) }
    view := bytes[1..4]
    for _ in 0 .. 64 { bytes << u8(255) }
    if view.len != 3 || view[0] != u8(97) || view[2] != u8(105) { C.exit(10) }
    if bytes.len != 76 || bytes[75] != u8(255) { C.exit(11) }
}
',
		]
		for building_v in [false, true] {
			mut paths := []string{}
			for index, source in sources {
				path := os.join_path(os.vtmp_dir(), 'arm64_enum_flags_${building_v}_${os.getpid()}_${index}.v')
				os.write_file(path, source)!
				paths << path
			}
			output := paths[0].all_before_last('.')
			defer {
				for path in paths { os.rm(path) or {} }
				os.rm(output) or {}
			}
			mut preferences := pref.new_preferences()
			preferences.backend = 'arm64'
			mut p := parser.Parser.new(preferences)
			mut a := p.parse_files(paths)
			assert p.diagnostics.len == 0, p.diagnostics.str()
			mut tc := types.TypeChecker.new(a)
			tc.building_v_fast = building_v
			tc.collect(a)
			if !building_v { tc.annotate_types() }
			assert tc.errors.len == 0, tc.errors.str()
			if building_v {
				_, _, errors := transform.transform_with_used_opt_config_scoped_workers_checked(mut a, tc, map[string]bool{}, false, true, false, true)
				assert errors.len == 0, errors.str()
			} else {
				transform.transform(mut a, tc)
			}
			m := ssa.build_with_used(a, map[string]bool{}, tc)
			mut g := Gen.new(m)
			g.gen()
			g.write_and_link(output)
			result := os.exec([output])
			assert result.exit_code == 0, 'building_v ${building_v}: exit ${result.exit_code}: ${result.output}'
		}
	}
}
