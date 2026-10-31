import os

fn test_imported_modules_retain_marked_native_declarations() ! {
	root := os.join_path(os.vtmp_dir(), 'module_extern_markused_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'bindings'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'bindings', 'bindings.c.v'), 'module bindings
@[markused; c_extern]
fn C.kept_native_symbol() int
pub fn anchor() int { return 7 }
')!
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import bindings\nfn main() { assert bindings.anchor() == 7 }\n')!
	output := os.join_path(root, 'main.c')
	for nocache in [true, false, false] {
		mut args := [@VEXE, '-new-compiler', '-b', 'c', '-gc', 'none', '-o', output]
		if nocache {
			args << '-nocache'
		}
		args << source
		result := os.exec(args)
		assert result.exit_code == 0, result.output
		assert os.read_file(output)!.split_into_lines().any(it.ends_with(' kept_native_symbol(void);'))
	}
}
