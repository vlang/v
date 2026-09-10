import os

const contextual_optional_vexe = @VEXE
const contextual_optional_tests_dir = os.dir(@FILE)
const contextual_optional_v3_dir = os.dir(contextual_optional_tests_dir)
const contextual_optional_vlib_dir = os.dir(contextual_optional_v3_dir)
const contextual_optional_v3_src = os.join_path(contextual_optional_v3_dir, 'v3.v')

fn test_contextual_optional_wraps_plain_call_and_if_once() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_contextual_optional_test')
	build :=
		os.execute('${contextual_optional_vexe} -gc none -d ownership -path "${contextual_optional_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${contextual_optional_v3_src}')
	assert build.exit_code == 0, build.output

	source := os.join_path(os.temp_dir(), 'v3_contextual_optional_input.v')
	os.write_file(source,
		"enum Mode {\n\tshort\n\tlong\n}\n\nstruct Bytes {\n\tvalue ?[]u8\n}\n\nstruct Args {\nmut:\n\tmode ?Mode\n}\n\nfn default_bytes() Bytes {\n\treturn Bytes{\n\t\tvalue: '--'.bytes()\n\t}\n}\n\nfn fail() ! {\n\treturn error('saved')\n}\n\nfn main() {\n\tbytes := default_bytes().value?\n\tassert bytes == [u8(`-`), `-`]\n\tmut args := Args{}\n\targs.mode = if bytes.len == 2 { .short } else { .long }\n\tassert args.mode? == .short\n\tmut saved := ?IError(none)\n\tfail() or { saved = err }\n\tassert (saved or { panic('missing error') }).msg() == 'saved'\n\tprintln('ok')\n}\n")!
	out := os.execute('${v3_bin} -ownership -d ownership -no-parallel run ${source}')
	assert out.exit_code == 0, out.output
	assert out.output.contains('\nok\n'), out.output
}
