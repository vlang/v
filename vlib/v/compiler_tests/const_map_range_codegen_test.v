import os
import v.tempname

struct ConstMapRangeCase {
	name     string
	source   string
	expected string
}

fn run_v3_const_map_range_programs(cases []ConstMapRangeCase) {
	root := os.join_path(os.vtmp_dir(), 'v3_const_map_range_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	mut imports := []string{cap: cases.len}
	mut calls := []string{cap: cases.len}
	for i, case in cases {
		module_name := 'fixture_${i}'
		module_dir := os.join_path(root, module_name)
		os.mkdir(module_dir) or { panic(err) }
		imports << 'import ${module_name}'
		calls << "\t\t'${case.name}' { ${module_name}.exercise() }"
		source := 'module ${module_name}\n\n' + case.source
		os.write_file(os.join_path(module_dir, 'v3_${case.name}.v'), source.replace_once('\nfn main()',
			'\npub fn exercise()')) or { panic(err) }
	}
	os.write_file(os.join_path(root, 'main.v'), "module main\n\nimport os\n${imports.join('\n')}\n\nfn main() {\n\tmatch os.args[1] {\n${calls.join('\n')}\n\t\telse { panic('unknown fixture') }\n\t}\n}\n") or {
		panic(err)
	}
	bin_path := os.join_path(root, 'program')
	compile := os.exec([@VEXE, '-enable-globals', '-b', 'c', '-o', bin_path, root])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	for case in cases {
		run := os.exec([bin_path, case.name])
		assert run.exit_code == 0, '${case.name}: ${run.output}'
		assert run.output.trim_space() == case.expected, '${case.name}: expected `${case.expected}` in\n${run.output}'
	}
}

fn test_const_map_initializer_and_range_high_bound_review_fixes() {
	run_v3_const_map_range_programs([
		ConstMapRangeCase{
			name:     'const_map_initializer'
			source:   "const m = map[string]int{\n\t'a': 1\n}\n\nfn main() {\n\tprintln(int_str(m['a']))\n}\n"
			expected: '1'
		},
		ConstMapRangeCase{
			name:     'const_array_interpolation'
			source:   "const values = [1, 2, 3]\nconst rendered_values = '\${values}'\n\nfn main() {\n\tprintln(rendered_values)\n}\n"
			expected: '[1, 2, 3]'
		},
		ConstMapRangeCase{
			name:     'range_high_once'
			source:   "__global calls int\n\nfn next_limit() int {\n\tcalls++\n\treturn 3\n}\n\nfn main() {\n\tmut total := 0\n\tfor i in 0 .. next_limit() {\n\t\ttotal += i\n\t}\n\tprintln(int_str(calls) + ':' + int_str(total))\n}\n"
			expected: '1:3'
		},
		ConstMapRangeCase{
			name:     'range_bound_order'
			source:   "__global events string\n\nfn low() int {\n\tevents += 'L'\n\treturn 0\n}\n\nfn high() int {\n\tevents += 'H'\n\treturn 2\n}\n\nfn main() {\n\tfor i in low() .. high() {\n\t\tevents += int_str(i)\n\t}\n\tprintln(events)\n}\n"
			expected: 'LH01'
		},
		ConstMapRangeCase{
			name:     'formatted_map_interpolation'
			source:   "fn main() {\n\tm := {\n\t\t'a': 1\n\t}\n\tprintln('\${m:10}')\n}\n"
			expected: "{'a': 1}"
		},
	])
}
