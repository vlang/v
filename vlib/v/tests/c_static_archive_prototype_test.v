import os

fn test_static_archive_supplies_headerless_c_prototype() {
	$if windows {
		return
	}
	cc := os.find_abs_path_of_executable('cc') or { return }
	ar := os.find_abs_path_of_executable('ar') or { return }
	root := os.join_path(os.vtmp_dir(), 'v_static_archive_prototype_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'ext.c')
	object := os.join_path(root, 'ext.o')
	archive := os.join_path(root, 'libext.a')
	main := os.join_path(root, 'main.v')
	os.write_file(source, 'void* v_archive_value(void) { return (void*)42; }\n')!
	compile := os.exec([cc, '-c', '-o', object, source])
	assert compile.exit_code == 0, compile.output
	link := os.exec([ar, 'rcs', archive, object])
	assert link.exit_code == 0, link.output
	for module_decl in ['', 'module main\n'] {
		os.write_file(main, module_decl + '#flag @DIR/libext.a\nfn C.v_archive_value() voidptr\n' +
			'fn main() { println(u64(C.v_archive_value())) }\n')!
		result := os.exec([@VEXE, '-cc', cc, 'run', main])
		assert result.exit_code == 0, result.output
		assert result.output.trim_space() == '42', result.output
	}
}
