import os
import v.cmdexec
import v.tempname

fn test_copyable_optional_can_be_read_and_assigned_repeatedly() {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}
	root := os.join_path(os.vtmp_dir(), 'v3_ownership_optional_copy_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'v3_ownership_optional_copy_input.v')
	os.write_file(source,
		"struct Cursor {\nmut:\n\tprevious ?rune\n}\n\nfn (mut cursor Cursor) replace(value ?rune) ?rune {\n\told := cursor.previous\n\tcursor.previous = value\n\treturn old\n}\n\nfn main() {\n\tmut cursor := Cursor{previous: rune(`a`)}\n\tfirst := cursor.previous\n\tassert first? == `a`\n\tassert cursor.previous? == `a`\n\tassert cursor.replace(rune(`b`))? == `a`\n\tassert cursor.previous? == `b`\n\tprintln('ok')\n}\n") or {
		panic(err)
	}
	out := cmdexec.run_in_merged_with_env(compiler, ['-ownership', '-d', 'ownership', '-no-parallel',
		'run', source], '', {
		'VFLAGS':               ''
		'VTOOLS_CACHE_VERBOSE': ''
	})
	assert out.exit_code == 0, out.output
	assert out.output.trim_space() == 'ok', out.output
}
