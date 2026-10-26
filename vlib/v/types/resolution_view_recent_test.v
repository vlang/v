module types

import v.flat

fn test_resolution_type_views_preserve_file_context_across_recent_hits_and_file_switches() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_file = 'caller.v'
	tc.cur_module = 'caller'
	tc.structs['left.Item'] = []StructField{}
	tc.structs['right.Item'] = []StructField{}
	tc.file_modules['left.v'] = 'left'
	tc.file_modules['right.v'] = 'right'
	for file in ['left.v', 'left.v'.clone(), 'right.v', 'right.v'.clone(), 'left.v'] {
		module_name := tc.file_modules[file] or { panic('missing file module') }
		for source in ['[]Item', '?Item', '&Item'] {
			fresh := tc.fork_type_parse_view(file, module_name)
			expected := fresh.parse_resolution_type(flat.source_type_id(source))
			assert tc.parse_resolution_type_in_file(flat.source_type_id(source), file) == expected
		}
	}
	assert tc.cur_file == 'caller.v'
	assert tc.cur_module == 'caller'
}

fn test_reset_resolution_type_views_discards_recent_file_context() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.structs['left.Item'] = []StructField{}
	tc.structs['right.Item'] = []StructField{}
	tc.file_modules['source.v'] = 'left'
	source := flat.source_type_id('[]Item')
	assert tc.parse_resolution_type_in_file(source, 'source.v').name() == '[]left.Item'
	tc.file_modules['source.v'] = 'right'
	tc.reset_resolution_type_view_cache()
	assert tc.parse_resolution_type_in_file(source, 'source.v').name() == '[]right.Item'
	tc.disable_resolution_type_view_cache()
	tc.file_modules['source.v'] = 'left'
	assert tc.parse_resolution_type_in_file(source, 'source.v').name() == '[]left.Item'
	assert isnil(tc.resolution_type_views)
}
