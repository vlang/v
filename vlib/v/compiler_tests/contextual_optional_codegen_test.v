// vtest ownership
import os
import v.cmdexec
import v.tempname

fn test_contextual_optional_wraps_plain_call_and_if_once() {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}
	root := os.join_path(os.vtmp_dir(), 'v3_contextual_optional_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'v3_contextual_optional_input.v')
	os.write_file(source,
		"enum Mode {\n\tshort\n\tlong\n}\n\nstruct Bytes {\n\tvalue ?[]u8\n}\n\nstruct Args {\nmut:\n\tmode ?Mode\n}\n\nfn default_bytes() Bytes {\n\treturn Bytes{\n\t\tvalue: '--'.bytes()\n\t}\n}\n\nfn fail() ! {\n\treturn error('saved')\n}\n\nfn main() {\n\tbytes := default_bytes().value?\n\tassert bytes == [u8(`-`), `-`]\n\tmut args := Args{}\n\targs.mode = if bytes.len == 2 { .short } else { .long }\n\tassert args.mode? == .short\n\tmut saved := ?IError(none)\n\tfail() or { saved = err }\n\tassert (saved or { panic('missing error') }).msg() == 'saved'\n\tprintln('ok')\n}\n")!
	out := cmdexec.run_in_merged_with_env(compiler, ['-ownership', '-d', 'ownership', '-no-parallel',
		'run', source], '', {
		'VFLAGS':               ''
		'VTOOLS_CACHE_VERBOSE': ''
	})
	assert out.exit_code == 0, out.output
	assert out.output.trim_space() == 'ok', out.output
}
