import os

// A project's own `modules/` directory declares modules for the whole project,
// so a module in it has to shadow a vlib module of the same name for an import
// written inside a submodule too, not only for one in the entry file.
//
// Only the importing file's own `modules/` used to be probed, so `import net`
// from `modules/wrapper/wrapper.v` reached past the project's `modules/net` and
// bound to vlib's `net` instead -- silently pulling vlib's `net`, `os`, `rand`
// and `io` into the build, and with them the whole hosted POSIX header set.
const project_files = {
	'main.v':                    'import wrapper\n\nfn main() {\n\tprintln(wrapper.describe())\n}\n'
	'modules/wrapper/wrapper.v': "module wrapper\n\nimport net\n\npub fn describe() string {\n\treturn 'wrapper saw ' + net.marker()\n}\n"
	'modules/net/net.v':         "module net\n\npub fn marker() string {\n\treturn 'the project net module'\n}\n"
}

fn setup_project(root string) {
	os.rmdir_all(root) or {}
	for path, content in project_files {
		full := os.join_path(root, path)
		os.mkdir_all(os.dir(full)) or { panic(err) }
		os.write_file(full, content) or { panic(err) }
	}
}

fn test_project_modules_dir_shadows_vlib_for_a_submodule_import() {
	os.setenv('VCOLORS', 'never', true)
	root := os.join_path(os.getwd(), '.tmp_local_module_shadows_vlib')
	setup_project(root)
	defer {
		os.rmdir_all(root) or {}
	}
	res := os.execute('${os.quoted_path(@VEXE)} run ${os.quoted_path(root)}')
	assert res.exit_code == 0, res.output
	assert res.output.replace('\r\n', '\n').trim_space() == 'wrapper saw the project net module', res.output
}

fn test_shadowed_vlib_module_is_not_compiled_in() {
	// Resolving the import to vlib's `net` also compiled vlib's `net` and its own
	// imports. Assert on the generated C so a regression cannot hide behind a
	// project that happens to still run.
	os.setenv('VCOLORS', 'never', true)
	root := os.join_path(os.getwd(), '.tmp_local_module_shadows_vlib_c')
	setup_project(root)
	c_path := os.join_path(root, 'shadow.c')
	defer {
		os.rmdir_all(root) or {}
	}
	res := os.execute('${os.quoted_path(@VEXE)} -o ${os.quoted_path(c_path)} ${os.quoted_path(root)}')
	assert res.exit_code == 0, res.output
	generated := os.read_file(c_path) or { panic(err) }
	assert generated.contains('the project net module'), generated#[-400..]
	for vlib_only_symbol in ['net__dial_tcp', 'net__TcpConn', 'net__socket_error'] {
		assert !generated.contains(vlib_only_symbol), vlib_only_symbol
	}
}
