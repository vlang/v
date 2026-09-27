module parser

import os
import v.pref

fn test_operator_overloads_do_not_collide_across_modules() {
	root := os.join_path(os.vtmp_dir(), 'operator_scope_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut p := Parser.new(pref.new_preferences())
	for name in ['first', 'second'] {
		path := os.join_path(root, '${name}.v')
		os.write_file(path, 'module ${name}\nstruct Color {}\nfn (a Color) + (b Color) Color { return a }\n')!
		p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
	}
}

fn test_duplicate_operator_overloads_in_one_file_are_rejected() {
	path := os.join_path(os.vtmp_dir(), 'duplicate_operator_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'struct Color {}\nfn (a Color) + (b Color) Color { return a }\nfn (a Color) + (b Color) Color { return b }\n')!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.diagnostics.len == 1, p.diagnostics.str()
	assert p.diagnostics[0].message == 'cannot duplicate operator overload `+`'
	assert p.diagnostics[0].line == 3
}
