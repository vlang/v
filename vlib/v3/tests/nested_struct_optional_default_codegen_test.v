import os

const nested_optional_default_vexe = @VEXE
const nested_optional_default_tests_dir = os.dir(@FILE)
const nested_optional_default_v3_dir = os.dir(nested_optional_default_tests_dir)
const nested_optional_default_vlib_dir = os.dir(nested_optional_default_v3_dir)
const nested_optional_default_v3_src = os.join_path(nested_optional_default_v3_dir, 'v3.v')

fn test_nested_struct_array_default_keeps_optional_wrapper() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_nested_optional_default_test')
	build :=
		os.execute('${nested_optional_default_vexe} -gc none -path "${nested_optional_default_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${nested_optional_default_v3_src}')
	assert build.exit_code == 0, build.output

	source := os.join_path(os.temp_dir(), 'v3_nested_optional_default_input.v')
	os.write_file(source,
		"enum PayloadKind {\n\tother\n}\n\nstruct Payload {\n\tkind PayloadKind = .other\n\ttext string\n\titems []int\n\tnested []Payload\n}\n\nstruct Middle {\n\tpayload ?Payload\n}\n\nstruct Outer {\n\tmiddle Middle\n}\n\nfn main() {\n\tvalues := []Outer{len: 2}\n\tassert values[0].middle.payload == none\n\tassert values[1].middle.payload == none\n\treserved := []Outer{cap: 2}\n\tassert reserved.len == 0\n\tassert reserved.cap >= 2\n\tprintln('ok')\n}\n")!
	out := os.execute('${v3_bin} -no-parallel run ${source}')
	assert out.exit_code == 0, out.output
	assert out.output.contains('\nok\n'), out.output
}
