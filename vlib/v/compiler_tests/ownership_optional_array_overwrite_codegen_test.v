import os
import v.cmdexec
import v.tempname

fn test_owned_optional_array_overwrite_wraps_cloned_payload() {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}
	root := os.join_path(os.vtmp_dir(), 'v3_optional_array_overwrite_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'v3_optional_array_overwrite_input.v')
	os.write_file(source,
		"struct Holder {\nmut:\n\tvalue ?[]u8\n\tseparator_value ?[]u8\n}\n\nfn (mut holder Holder) set(value ?[]u8) {\n\tif bytes := value {\n\t\tholder.value = bytes.clone()\n\t} else {\n\t\tholder.value = none\n\t}\n}\n\nfn (mut holder Holder) separator(separator ?[]u8) {\n\tif sep := separator {\n\t\tholder.separator_value = sep.clone()\n\t} else {\n\t\tholder.separator_value = none\n\t}\n}\n\nfn main() {\n\tmut holder := Holder{}\n\tholder.set([u8(4), 2])\n\tassert holder.value? == [u8(4), 2]\n\tholder.separator([u8(5), 3])\n\tassert holder.separator_value? == [u8(5), 3]\n\tprintln('ok')\n}\n")!
	out := cmdexec.run_in_merged_with_env(compiler, ['-ownership', '-d', 'ownership', '-no-parallel',
		'run', source], '', {
		'VFLAGS':               ''
		'VTOOLS_CACHE_VERBOSE': ''
	})
	assert out.exit_code == 0, out.output
	assert out.output.trim_space() == 'ok', out.output
}
