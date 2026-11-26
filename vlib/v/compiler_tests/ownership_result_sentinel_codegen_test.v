import os
import v.cmdexec
import v.tempname

fn test_ownership_generated_result_cleanup_roots_ierror_sentinels() {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}
	env := {
		'VFLAGS':               ''
		'VTOOLS_CACHE_VERBOSE': ''
	}
	root := os.join_path(os.vtmp_dir(), 'v3_ownership_sentinel_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'v3_ownership_sentinel_input.v')
	os.write_file(source,
		"struct Node {\n\tvalue ?string\n\tchildren []Node\n}\n\nfn main() {\n\tnode := Node{\n\t\tvalue: 'kept'\n\t\tchildren: []Node{}\n\t}\n\tassert node.value or { '' } == 'kept'\n\tprintln('ok')\n}\n")!
	out :=
		cmdexec.run_in_merged_with_env(compiler, ['-ownership', '-d', 'ownership', '-no-parallel',
			'-macos-v3-compat-c99', 'run', source], '', env)
	assert out.exit_code == 0, out.output
	assert out.output.trim_space() == 'ok', out.output

	os.write_file(source,
		"fn main() {\n\t\$if ownership ? {\n\t\tprintln('ownership')\n\t} \$else {\n\t\tprintln('standard')\n\t}\n}\n")!
	autofree := cmdexec.run_in_merged_with_env(compiler, ['-ownership', '-autofree', '-d', 'ownership',
		'-no-parallel', 'run', source], '', env)
	assert autofree.exit_code == 0, autofree.output
	assert autofree.output.trim_space() == 'ownership', autofree.output

	os.write_file(source,
		"fn make_value() !string {\n\terror('discarded')\n\treturn 'ok'\n}\n\nfn main() {\n\tprintln(make_value() or { panic(err) })\n}\n")!
	discarded_error :=
		cmdexec.run_in_merged_with_env(compiler, ['-ownership', '-autofree', '-d', 'ownership',
			'-no-parallel', 'run', source], '', env)
	assert discarded_error.exit_code == 0, discarded_error.output
	assert discarded_error.output.trim_space() == 'ok', discarded_error.output
}
