// vtest ownership
import os
import v.cmdexec
import v.tempname

fn test_owned_array_mutations_keep_generated_drop_loops_well_formed() {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}
	root := os.join_path(os.vtmp_dir(), 'v3_owned_array_mutation_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'v3_owned_array_mutation_input.v')
	os.write_file(source,
		"fn main() {\n\tmut values := ['a', 'b', 'c']\n\tvalues.delete(1)\n\tassert values == ['a', 'c']\n\tvalues.clear()\n\tassert values.len == 0\n\tvalues = ['last']\n\tvalues.free()\n\tassert values.len == 0\n\tmut nested := [['owned']]\n\tnested.pop()\n\tassert nested.len == 0\n\tprintln('ok')\n}\n")!
	out := cmdexec.run_in_merged_with_env(compiler, ['-ownership', '-d', 'ownership', '-no-parallel',
		'run', source], '', {
		'VFLAGS':               ''
		'VTOOLS_CACHE_VERBOSE': ''
	})
	assert out.exit_code == 0, out.output
	assert out.output.contains('\nok\n'), out.output
}
