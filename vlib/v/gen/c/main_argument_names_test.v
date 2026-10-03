module c

import os

fn test_main_locals_can_use_argument_names() {
	root := os.join_path(os.vtmp_dir(), 'main_argument_names_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	body := 'mut argc := 7
mut argv := "local"
argc += 1
argv += "-value"
assert argc == 8
assert argv == "local-value"
assert os.args.len == 3
assert os.args[1..] == ["first", "second"]
assert argument_names(argc, argv) == "8:local-value"
'
	helper := 'fn argument_names(argc int, argv string) string {
return "\${argc}:\${argv}"
}
'
	for index, source in [
		'module main\nimport os\nfn main() {\n${body}}\n${helper}',
		'import os\n${helper}\n${body}',
		'@[translated]\nmodule main\nimport os\nfn main() {\n${body}}\n${helper}',
	] {
		path := os.join_path(root, 'main_${index}.v')
		os.write_file(path, source)!
		for flags in ['', '-no-parallel -nocache'] {
			executable := os.join_path(root, 'program_${index}')
			build := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-gc', 'none',
				'-o', executable, path])
			assert build.exit_code == 0, build.output
			run := os.exec([executable, 'first', 'second'])
			assert run.exit_code == 0, run.output
		}
	}
}
