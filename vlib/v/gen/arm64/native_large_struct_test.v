module arm64

import os
import strings
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_constructor_returns_more_than_256_mixed_fields() {
	$if macos && arm64 {
		mut source := strings.new_builder(16000)
		source.writeln('module main\nfn C.exit(int)\nstruct ManyFields {\n anchor &int = unsafe { nil }')
		field_types := ['i64', '[]i64', 'map[string]int', 'u8']
		for i in 0 .. 300 {
			source.writeln(' field${i} ${field_types[i % 4]}')
		}
		source.writeln('}
fn create(anchor &int, value i64) ManyFields {
    return ManyFields{anchor: anchor, field0: value, field299: 99}
}
fn relay(anchor &int, value i64) ManyFields { return create(anchor, value) }
fn observe(value ManyFields) int {
    if value.field149.len != 0 || value.field150.len != 0 { C.exit(4) }
    if value.field296 != 0 || value.field297.len != 0 { C.exit(5) }
    return int(value.field0) + int(value.field299)
}
fn main() {
    marker := 37
    first := relay(&marker, 11)
    second := relay(&marker, 23)
    if first.anchor != &marker || second.anchor != &marker { C.exit(1) }
    if *first.anchor != 37 || *second.anchor != 37 { C.exit(2) }
    if first.field0 != 11 || second.field0 != 23 { C.exit(3) }
    if observe(first) != 110 || observe(second) != 122 { C.exit(6) }
}
')
		path := os.join_path(os.vtmp_dir(), 'arm64_many_fields_${os.getpid()}.v')
		output := path.all_before_last('.')
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
		}
		os.write_file(path, source.str()) or { panic(err) }
		mut preferences := pref.new_preferences()
		preferences.backend = 'arm64'
		mut p := parser.Parser.new(preferences)
		mut a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := types.TypeChecker.new(a)
		tc.collect(a)
		tc.annotate_types()
		assert tc.errors.len == 0, tc.errors.str()
		transform.transform(mut a, tc)
		m := ssa.build_with_used(a, map[string]bool{}, tc)
		constructors := m.funcs.filter(it.name == 'create')
		assert constructors.len == 1
		assert m.type_store.types[constructors[0].typ].fields.len == 301
		assert m.type_size(constructors[0].typ) == 4208
		mut g := Gen.new(m)
		g.gen()
		g.write_and_link(output)
		result := os.exec([output])
		assert result.exit_code == 0, result.output
	}
}
