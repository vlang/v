import os

fn test_nested_module_lookup_works_with_relative_paths() {
	os.setenv('VCOLORS', 'never', true)
	wd := os.getwd()
	root_name := '.tmp_relative_nested_module_lookup'
	root := os.join_path(wd, root_name)
	main_path := os.join_path(root, 'main.v')
	rel_main_path := os.join_path(root_name, 'main.v')
	mod_dir := os.join_path(root, 'mod')
	v2_dir := os.join_path(mod_dir, 'v2')
	os.rmdir_all(root) or {}
	os.mkdir_all(v2_dir) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(main_path, 'import mod\nimport mod.v2\n\nfn main() {\n\tmod.print_version()\n\tv2.print_version()\n}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(mod_dir, 'mod.v'), "module mod\n\nconst version = 'v1'\n\npub fn print_version() {\n\tprintln(mod.version)\n}\n") or {
		panic(err)
	}
	os.write_file(os.join_path(v2_dir, 'mod.v'), "module v2\n\nconst version = 'v2'\n\npub fn print_version() {\n\tprintln(v2.version)\n}\n") or {
		panic(err)
	}
	cmd := '${os.quoted_path(@VEXE)} run ${os.quoted_path(rel_main_path)}'
	res := os.execute(cmd)
	assert res.exit_code == 0, res.output
	assert res.output.replace('\r\n', '\n').trim_space() == 'v1\nv2'
}
