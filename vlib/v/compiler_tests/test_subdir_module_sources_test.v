import os
import v.cmdexec

fn test_subdir_test_files_include_root_and_sibling_sources() {
	for base_url in ['', 'src'] {
		root := os.join_path(os.vtmp_dir(), 'v3_subdir_test_sources_${os.getpid()}_${base_url}')
		source_root := if base_url == '' { root } else { os.join_path(root, base_url) }
		os.mkdir_all(os.join_path(source_root, 'tests'))!
		os.mkdir_all(os.join_path(source_root, 'parts'))!
		defer { os.rmdir_all(root) or {} }
		os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'widget', base_url: '${base_url}', subdirs: ['tests', 'parts'] }\n")!
		os.write_file(os.join_path(source_root, 'lib.v'), 'module widget\npub fn root_fn() int { return 40 }\n')!
		os.write_file(os.join_path(source_root, 'parts', 'part.v'), 'module widget\npub fn part_fn() int { return 2 }\n')!
		test_file := os.join_path(source_root, 'tests', 'a_test.v')
		os.write_file(test_file, 'module widget\nfn test_whole_module() { assert root_fn() + part_fn() == 42 }\n')!
		other_test := os.join_path(source_root, 'parts', 'unselected_test.v')
		os.write_file(other_test, 'module widget\nfn test_unselected() { assert false }\n')!
		for args in [['-new-compiler', test_file], ['-new-compiler', 'test', test_file]] {
			result := cmdexec.run(@VEXE, args)
			assert result.exit_code == 0, result.output
		}
		os.rm(other_test)!
		all_tests := cmdexec.run(@VEXE, ['-new-compiler', 'test', root])
		assert all_tests.exit_code == 0, all_tests.output
	}
}
