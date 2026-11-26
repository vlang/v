// vtest ownership
import os
import v.cmdexec
import v.tempname

fn test_owned_map_delete_snapshots_and_drops_the_stored_value() {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}
	root := os.join_path(os.vtmp_dir(), 'v3_owned_map_delete_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'v3_owned_map_delete_input.v')
	os.write_file(source,
		"fn main() {\n\tmut values := map[string][]string{}\n\tvalues['letters'] = ['a', 'b']\n\tvalues.delete('letters')\n\tassert values.len == 0\n\tprintln('ok')\n}\n")!
	out := cmdexec.run_in_merged_with_env(compiler, ['-ownership', '-d', 'ownership', '-no-parallel',
		'run', source], '', {
		'VFLAGS':               ''
		'VTOOLS_CACHE_VERBOSE': ''
	})
	assert out.exit_code == 0, out.output
	assert out.output.split_into_lines().contains('ok'), out.output
}
