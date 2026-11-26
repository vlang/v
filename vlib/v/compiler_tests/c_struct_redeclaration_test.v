import os
import v.tempname

fn c_struct_redecl_fixture_dir(name string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_${name}_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	return root
}

fn c_struct_redecl_compile_project(name string, files map[string]string) os.Result {
	root := c_struct_redecl_fixture_dir('${name}_project')
	defer { os.rmdir_all(root) or {} }
	for rel, src in files {
		path := os.join_path(root, rel)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, src) or { panic(err) }
	}
	out := os.join_path(root, 'v3_${name}')
	return os.exec([@VEXE, root, '-b', 'c', '-o', '${out}'])
}

fn test_c_struct_redeclaration_checks_field_signature() {
	accepted := c_struct_redecl_compile_project('accepted_c_struct_redeclarations', {
		'main.v':                                         'module main\n\nfn main() {}\n'
		'good_c_struct_redeclaration.v':                  'module main\n\n@[typedef]\nstruct C.XKeyEvent {\n\tkeycode u32\n\tstate u32\n}\n\nstruct C.XKeyEvent {\n\tkeycode u32\n\tstate u32\n}\n\n@[typedef]\nunion C.XClientMessageData {\nmut:\n\tb [20]u8\n\ts [10]i16\n\tl [5]i64\n}\n\nunion C.XClientMessageData {\nmut:\n\tl [5]i64\n}\n\n@[typedef]\nunion C.XEvent {\nmut:\n\ttype int\n\txkey C.XKeyEvent\n}\n\nunion C.XEvent {\nmut:\n\t@type int\n\txkey C.XKeyEvent\n}\n'
		'good_c_struct_partial_views.v':                  'module main\n\n@[typedef]\nstruct C.HeaderEvent {\n\ttype int\n\tserial u64\n\tdisplay voidptr\n\twindow u64\n\tatom u64\n\tstate int\n}\n\nstruct C.HeaderEvent {\n\tstate int\n\tatom u64\n}\n\n@[typedef]\nunion C.HeaderUnion {\n\ttype int\n\tclient C.HeaderEvent\n\tpad [24]i64\n}\n\nunion C.HeaderUnion {\n\t@type int\n\tproperty C.HeaderEvent\n}\n'
		'good_c_struct_alias_field_redeclaration.v':      'module main\n\ntype MyByte = u8\n\nstruct C.Foo {\n\tx MyByte\n}\n\nstruct C.Foo {\n\tx u8\n}\n'
		'good_c_struct_late_alias_field_redeclaration.v': 'module main\n\nstruct C.LateAlias {\n\tx MyInt\n}\n\ntype MyInt = int\n\nstruct C.LateAlias {\n\tx int\n}\n'
	})
	assert accepted.exit_code == 0, accepted.output
	assert !accepted.output.contains('C compilation failed'), accepted.output

	rejected := c_struct_redecl_compile_project('rejected_c_struct_redeclarations', {
		'main.v':                           'module main\n\nfn main() {}\n'
		'bad_c_struct_redeclaration.v':     'module main\n\nstruct C.dirent {\n\td_name [256]char\n}\n\nstruct C.dirent {\n\td_name byteptr\n}\n'
		'bad_c_struct_reordered_fields.v':  'module main\n\nstruct C.OrderedPair {\n\tleft int\n\tright int\n}\n\nstruct C.OrderedPair {\n\tright int\n\tleft int\n}\n'
		'bad_c_struct_partial_view_type.v': 'module main\n\nstruct C.HeaderEvent {\n\ttype int\n\tserial u64\n\tstate int\n}\n\nstruct C.HeaderEvent {\n\tstate u64\n}\n'
	})
	assert rejected.exit_code != 0, rejected.output
	assert rejected.output.contains('cannot redeclare C struct `C.dirent`'), rejected.output
	assert rejected.output.contains('cannot redeclare C struct `C.OrderedPair`'), rejected.output
	assert rejected.output.contains('cannot redeclare C struct `C.HeaderEvent`'), rejected.output
	assert !rejected.output.contains('C compilation failed'), rejected.output

	cross_file_bad := c_struct_redecl_compile_project('bad_c_struct_cross_file_redeclaration', {
		'a.v': 'module main\n\nstruct C.Split {\n\tx int\n}\n\nfn main() {}\n'
		'b.v': 'module main\n\nstruct C.Split {\n\tx string\n}\n'
	})
	assert cross_file_bad.exit_code != 0, cross_file_bad.output
	assert cross_file_bad.output.contains('cannot redeclare C struct `C.Split`'), cross_file_bad.output
	assert !cross_file_bad.output.contains('C compilation failed'), cross_file_bad.output

	cross_module_bad := c_struct_redecl_compile_project('bad_c_struct_cross_module_redeclaration', {
		'v.mod':  'Module { name: "shared_header_modules" }\n'
		'a/a.v':  'module a\n\npub struct C.SharedHeader {\n\tx int\n}\n\npub fn touch_a() int {\n\treturn 1\n}\n'
		'b/b.v':  'module b\n\npub struct C.SharedHeader {\n\ty byteptr\n}\n\npub fn touch_b() int {\n\treturn 2\n}\n'
		'main.v': 'module main\n\nimport a\nimport b\n\nfn main() {\n\t$compile_error("semantic checking continued after C declaration conflict")\n\tprintln(int_str(a.touch_a() + b.touch_b()))\n}\n'
	})
	assert cross_module_bad.exit_code != 0, cross_module_bad.output
	assert cross_module_bad.output.contains('cannot redeclare C struct `C.SharedHeader`'), cross_module_bad.output
	assert cross_module_bad.output.contains(os.path_separator + os.join_path('b', 'b.v') + ':3:'), cross_module_bad.output
	assert !cross_module_bad.output.contains('semantic checking continued after C declaration conflict'), cross_module_bad.output

	assert !cross_module_bad.output.contains('C compilation failed'), cross_module_bad.output

	cached_termios_good := c_struct_redecl_compile_project('module_cache_termios_shims', {
		'v.mod':                   'Module { name: "cached_termios_shims" }\n'
		'term/term_cached.vh':     'module term\n\nstruct C.termios {\n\tc_iflag int\n}\n'
		'term/termios/termios.vh': 'module termios\n\nstruct C.termios {\n\tc_iflag uint\n\tc_ispeed uint\n}\n'
		'main.v':                  'module main\n\nimport term\nimport term.termios\n\nfn main() {}\n'
	})
	assert cached_termios_good.exit_code == 0, cached_termios_good.output
	assert !cached_termios_good.output.contains('cannot redeclare C struct `C.termios`'), cached_termios_good.output
}
