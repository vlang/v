import os
import v.tempname

fn run_v3_const_map_range_program(name string, src string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_${name}_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	src_path := os.join_path(root, 'v3_${name}.v')
	bin_path := os.join_path(root, 'v3_${name}_bin')
	os.write_file(src_path, src) or { panic(err) }
	compile := os.exec([@VEXE, '-enable-globals', src_path, '-b', 'c', '-o', bin_path])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin_path])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_const_map_initializer_and_range_high_bound_review_fixes() {
	const_map := run_v3_const_map_range_program('const_map_initializer',
		"const m = map[string]int{\n\t'a': 1\n}\n\nfn main() {\n\tprintln(int_str(m['a']))\n}\n")
	assert const_map == '1'
	const_array_interp := run_v3_const_map_range_program('const_array_interpolation',
		"const values = [1, 2, 3]\nconst rendered_values = '\${values}'\n\nfn main() {\n\tprintln(rendered_values)\n}\n")
	assert const_array_interp == '[1, 2, 3]'
	range_high := run_v3_const_map_range_program('range_high_once',
		"__global calls int\n\nfn next_limit() int {\n\tcalls++\n\treturn 3\n}\n\nfn main() {\n\tmut total := 0\n\tfor i in 0 .. next_limit() {\n\t\ttotal += i\n\t}\n\tprintln(int_str(calls) + ':' + int_str(total))\n}\n")
	assert range_high == '1:3'
	range_order := run_v3_const_map_range_program('range_bound_order',
		"__global events string\n\nfn low() int {\n\tevents += 'L'\n\treturn 0\n}\n\nfn high() int {\n\tevents += 'H'\n\treturn 2\n}\n\nfn main() {\n\tfor i in low() .. high() {\n\t\tevents += int_str(i)\n\t}\n\tprintln(events)\n}\n")
	assert range_order == 'LH01'
	formatted_map := run_v3_const_map_range_program('formatted_map_interpolation',
		"fn main() {\n\tm := {\n\t\t'a': 1\n\t}\n\tprintln('\${m:10}')\n}\n")
	assert formatted_map == "{'a': 1}"
}
