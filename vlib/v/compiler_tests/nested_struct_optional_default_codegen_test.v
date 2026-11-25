import os
import v.tempname

fn test_nested_struct_array_default_keeps_optional_wrapper() {
	dir := os.join_path(os.vtmp_dir(), 'v3_nested_optional_default_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'v3_nested_optional_default_input.v')
	os.write_file(source,
		"enum PayloadKind {\n\tother\n}\n\nstruct Payload {\n\tkind PayloadKind = .other\n\ttext string\n\titems []int\n\tnested []Payload\n}\n\nstruct Middle {\n\tpayload ?Payload\n}\n\nstruct Outer {\n\tmiddle Middle\n}\n\nfn main() {\n\tvalues := []Outer{len: 2}\n\tassert values[0].middle.payload == none\n\tassert values[1].middle.payload == none\n\treserved := []Outer{cap: 2}\n\tassert reserved.len == 0\n\tassert reserved.cap >= 2\n\tprintln('ok')\n}\n")!
	out := os.exec([@VEXE, '-no-parallel', 'run', source])
	assert out.exit_code == 0, out.output
	assert out.output.split_into_lines().contains('ok'), out.output
}
