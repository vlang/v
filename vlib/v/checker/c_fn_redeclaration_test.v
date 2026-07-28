import os

const c_fn_redeclaration_vexe = os.quoted_path(@VEXE)

fn write_c_fn_redeclaration_project(name string, files map[string]string) !string {
	root := os.join_path(os.vtmp_dir(), '${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	for relative_path, source in files {
		path := os.join_path(root, relative_path)
		os.mkdir_all(os.dir(path))!
		os.write_file(path, source)!
	}
	return root
}

fn test_conflicting_c_fn_redeclarations_across_modules_are_reported_at_declaration() {
	root := write_c_fn_redeclaration_project('c_fn_conflicting_redeclaration', {
		'v.mod':       "Module { name: 'c_fn_conflicting_redeclaration' }\n"
		'moda/moda.v': 'module moda\n\nfn C.getpid() int\n\npub fn pid() int {\n\treturn C.getpid()\n}\n'
		'modb/modb.v': 'module modb\n\nfn C.getpid() u64\n\npub fn pid() u64 {\n\treturn C.getpid()\n}\n'
		'main.v':      'module main\n\nimport moda\nimport modb\n\nfn main() {\n\tprintln(moda.pid())\n\tprintln(modb.pid())\n}\n'
	})!
	defer {
		os.rmdir_all(root) or {}
	}
	result := os.execute('${c_fn_redeclaration_vexe} -check ${os.quoted_path(root)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('moda/moda.v:3:') || result.output.contains('modb/modb.v:3:')
	assert result.output.contains('C function `C.getpid` was already declared with a different signature')
	assert !result.output.contains('cannot use `u64` as `int`'), result.output
}

fn test_compatible_c_fn_redeclarations_across_modules_are_accepted() {
	root := write_c_fn_redeclaration_project('c_fn_compatible_redeclaration', {
		'v.mod':       "Module { name: 'c_fn_compatible_redeclaration' }\n"
		'moda/moda.v': 'module moda\n\nstruct C.Display {}\n\nfn C.compat_probe(value int, data byteptr, display voidptr, mut state usize) int\n\npub fn touch() {\n\tmut state := usize(0)\n\tC.compat_probe(0, unsafe { nil }, unsafe { nil }, mut state)\n}\n'
		'modb/modb.v': 'module modb\n\nstruct C.Display {}\n\ntype CInt = i32\n\nfn C.compat_probe(value CInt, data &u8, display &C.Display, state &usize) i32\n\npub fn touch() {\n\tmut state := usize(0)\n\tC.compat_probe(0, unsafe { nil }, unsafe { nil }, &state)\n}\n'
		'main.v':      'module main\n\nimport moda\nimport modb\n\nfn main() {\n\tmoda.touch()\n\tmodb.touch()\n}\n'
	})!
	defer {
		os.rmdir_all(root) or {}
	}
	result := os.execute('${c_fn_redeclaration_vexe} -check ${os.quoted_path(root)}')
	assert result.exit_code == 0, result.output
}

fn test_fixed_and_variadic_c_fn_redeclarations_conflict() {
	root := write_c_fn_redeclaration_project('c_fn_fixed_variadic_redeclaration', {
		'v.mod':       "Module { name: 'c_fn_fixed_variadic_redeclaration' }\n"
		'moda/moda.v': 'module moda\n\nfn C.variadic_probe(value int, weight f32) int\n\npub fn touch() {}\n'
		'modb/modb.v': 'module modb\n\nfn C.variadic_probe(value int, ...) int\n\npub fn touch() {}\n'
		'main.v':      'module main\n\nimport moda\nimport modb\n\nfn main() {\n\tmoda.touch()\n\tmodb.touch()\n}\n'
	})!
	defer {
		os.rmdir_all(root) or {}
	}
	result := os.execute('${c_fn_redeclaration_vexe} -check ${os.quoted_path(root)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('C function `C.variadic_probe` was already declared with a different signature'), result.output
}

fn test_gg_and_clipboard_c_fn_redeclarations_are_compatible() {
	root := write_c_fn_redeclaration_project('c_fn_gg_clipboard_redeclaration', {
		'main.v': 'module main\n\nimport gg as _\nimport clipboard as _\n\nfn main() {}\n'
	})!
	defer {
		os.rmdir_all(root) or {}
	}
	result := os.execute('${c_fn_redeclaration_vexe} -os linux -check ${os.quoted_path(root)}')
	assert result.exit_code == 0, result.output
}

fn test_clipboard_and_x11_c_fn_redeclarations_are_compatible() {
	root := write_c_fn_redeclaration_project('c_fn_clipboard_x11_redeclaration', {
		'main.v': 'module main\n\nimport clipboard as _\nimport x.x11 as _\n\nfn main() {}\n'
	})!
	defer {
		os.rmdir_all(root) or {}
	}
	result := os.execute('${c_fn_redeclaration_vexe} -os linux -check ${os.quoted_path(root)}')
	assert result.exit_code == 0, result.output
}
