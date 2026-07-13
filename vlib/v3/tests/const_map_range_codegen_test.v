import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn tmp_const_map_range_path(name string) string {
	return os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
}

fn build_v3_const_map_range() string {
	v3_bin := tmp_const_map_range_path('const_map_range')
	build :=
		os.execute('${os.quoted_path(vexe)} -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(v3_bin)} ${os.quoted_path(v3_src)}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn run_v3_const_map_range_program(v3_bin string, name string, src string) string {
	src_path := '${tmp_const_map_range_path(name)}.v'
	bin_path := tmp_const_map_range_path('${name}_bin')
	os.write_file(src_path, src) or { panic(err) }
	compile :=
		os.execute('${os.quoted_path(v3_bin)} ${os.quoted_path(src_path)} -b c -o ${os.quoted_path(bin_path)}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(os.quoted_path(bin_path))
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_const_map_initializer_and_range_high_bound_review_fixes() {
	v3_bin := build_v3_const_map_range()
	const_map := run_v3_const_map_range_program(v3_bin, 'const_map_initializer',
		"const m = map[string]int{\n\t'a': 1\n}\n\nfn main() {\n\tprintln(int_str(m['a']))\n}\n")
	assert const_map == '1'
	const_array_interp := run_v3_const_map_range_program(v3_bin, 'const_array_interpolation',
		"const values = [1, 2, 3]\nconst rendered_values = '\${values}'\n\nfn main() {\n\tprintln(rendered_values)\n}\n")
	assert const_array_interp == '[1, 2, 3]'
	range_high := run_v3_const_map_range_program(v3_bin, 'range_high_once',
		"__global calls int\n\nfn next_limit() int {\n\tcalls++\n\treturn 3\n}\n\nfn main() {\n\tmut total := 0\n\tfor i in 0 .. next_limit() {\n\t\ttotal += i\n\t}\n\tprintln(int_str(calls) + ':' + int_str(total))\n}\n")
	assert range_high == '1:3'
	range_order := run_v3_const_map_range_program(v3_bin, 'range_bound_order',
		"__global events string\n\nfn low() int {\n\tevents += 'L'\n\treturn 0\n}\n\nfn high() int {\n\tevents += 'H'\n\treturn 2\n}\n\nfn main() {\n\tfor i in low() .. high() {\n\t\tevents += int_str(i)\n\t}\n\tprintln(events)\n}\n")
	assert range_order == 'LH01'
	formatted_map := run_v3_const_map_range_program(v3_bin, 'formatted_map_interpolation',
		"fn main() {\n\tm := {\n\t\t'a': 1\n\t}\n\tprintln('\${m:10}')\n}\n")
	assert formatted_map == "{'a': 1}"
}
