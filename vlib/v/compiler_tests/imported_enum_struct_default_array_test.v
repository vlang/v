import os

fn test_imported_enum_default_in_fixed_array() {
	root := os.join_path(os.temp_dir(), 'v3_imported_enum_array_${os.getpid()}')
	dep_dir := os.join_path(root, 'dep')
	main_path := os.join_path(root, 'main.v')
	output_path := os.join_path(root, 'program')
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(dep_dir) or { panic(err) }
	os.write_file(os.join_path(dep_dir, 'dep.v'), 'module dep\npub enum Kind { first second }\npub struct Item {\npub mut:\n\tkind Kind = Kind.first\n}\n') or { panic(err) }
	os.write_file(main_path, 'module main\nimport dep\nfn main() {\n\titems := [2]dep.Item{}\n\tassert items[0].kind == .first\n\tassert items[1].kind == .first\n}\n') or { panic(err) }
	compile := os.execute('${os.quoted_path(@VEXE)} -new-compiler -path "${root}|@vlib|@vmodules" -o ${os.quoted_path(output_path)} ${os.quoted_path(main_path)}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(os.quoted_path(output_path))
	assert run.exit_code == 0, run.output
}
