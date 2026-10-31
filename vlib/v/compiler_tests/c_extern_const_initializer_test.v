import os

fn test_c_extern_called_from_imported_const_has_prototype() {
	root := os.join_path(os.vtmp_dir(), 'v3_c_extern_const_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'hidden')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'hidden', 'hidden.c.v'), 'module hidden\n\n#flag -lhidden\nfn C.hidden_external() int\n\npub const hidden_value = helper() + C.hidden_external()\n\nfn helper() int {\n\treturn 7\n}\n') or {
		panic(err)
	}
	main_path := os.join_path(root, 'main.v')
	os.write_file(main_path, 'module main\n\nimport hidden\n\nfn main() {\n\tprintln(hidden.hidden_value)\n}\n') or {
		panic(err)
	}
	out_path := os.join_path(root, 'out.c')
	result := os.exec([@VEXE, '-new-compiler', '-gc', 'none', '-nocache', '-o', out_path, main_path])
	assert result.exit_code == 0, result.output
	c_code := os.read_file(out_path) or { panic(err) }
	assert c_code.contains('int hidden_external(void);'), 'missing C prototype for const initializer'
	assert c_code.contains('hidden__hidden_value = hidden__helper() + hidden_external();')
}
