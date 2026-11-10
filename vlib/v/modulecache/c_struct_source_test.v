module modulecache

import v.parser
import v.pref
import v.types

fn test_c_struct_source_metadata_preserves_escaped_source_paths() {
	native_source := "C:\\project files\\quoted's\\native.c.v"
	owned_source := '/project/line\nbreak/owned.v'
	mut p := parser.Parser.new(pref.new_preferences())
	p.parse_text(native_source, 'module binding\npub struct C.Native {}\n')
	a := p.parse_text(owned_source, 'module binding\npub struct C.Owned {}\npub struct Ordinary {}\n')
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	header := module_header_with_const_order(a, &tc, 'binding', '/project', map[string]string{},
		[]string{})
	sources := header_c_struct_sources(header)
	assert sources.len == 2
	assert sources['C.Native'] == native_source
	assert sources['C.Owned'] == owned_source
	assert header.split_into_lines().filter(it.starts_with(c_struct_source_marker)).len == 2
}
