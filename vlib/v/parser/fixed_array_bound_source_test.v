module parser

import os
import v.pref

fn test_fixed_array_bounds_preserve_comptime_source_without_node_spans() {
	key := 'V3_FIXED_ARRAY_BOUND_SOURCE_TEST'
	previous := os.getenv(key)
	defer { os.setenv(key, previous, true) }
	path := os.join_path(os.vtmp_dir(), 'fixed_array_bound_source_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	source := "struct Holder {\n data [\$env('${key}')]int\n}\n"
	os.write_file(path, source)!
	for value in ['', '4'] {
		os.setenv(key, value, true)
		mut p := Parser.new(pref.new_preferences())
		p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut found := false
		for node in p.a.nodes {
			if node.kind == .field_decl && node.value == 'data' {
				assert node.typ == "[\$env('${key}')]int", node.typ
				found = true
			}
		}
		assert found
	}
}
