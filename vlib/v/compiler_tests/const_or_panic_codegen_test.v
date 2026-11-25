import os
import v.tempname

struct ConstOrPanicCase {
	name     string
	source   string
	expected string
}

fn run_v3_const_or_panic_programs(cases []ConstOrPanicCase) {
	root := os.join_path(os.vtmp_dir(), 'v3_const_or_panic_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	mut imports := []string{cap: cases.len}
	mut calls := []string{cap: cases.len}
	for case in cases {
		module_dir := os.join_path(root, case.name)
		os.mkdir(module_dir) or { panic(err) }
		imports << 'import ${case.name}'
		calls << "\t\t'${case.name}' { ${case.name}.exercise() }"
		fixture := 'module ${case.name}\n\n' + case.source
		os.write_file(os.join_path(module_dir, 'v3_${case.name}.v'), fixture.replace_once('\nfn main()',
			'\npub fn exercise()')) or { panic(err) }
	}
	os.write_file(os.join_path(root, 'main.v'), "module main\n\nimport os\n${imports.join('\n')}\n\nfn main() {\n\tmatch os.args[1] {\n${calls.join('\n')}\n\t\telse { panic('unknown fixture') }\n\t}\n}\n") or {
		panic(err)
	}
	bin_path := os.join_path(root, 'program')
	compile := os.exec([@VEXE, '-b', 'c', '-o', bin_path, root])
	assert compile.exit_code == 0, compile.output
	// The regressed path emitted `gen_node: unsupported node kind: call` to stderr
	// while still producing a binary, so assert the diagnostic is absent too.
	assert !compile.output.contains('unsupported node kind'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	for case in cases {
		run := os.exec([bin_path, case.name])
		assert run.exit_code == 0, '${case.name}: ${run.output}'
		assert run.output.trim_space() == case.expected, '${case.name}: expected `${case.expected}` in\n${run.output}'
	}
}

// A const initialized by a Result-returning call guarded with `or { panic(..) }`
// used to reach the C backend as a bare `.call` node in the or-body tail, which
// gen_node could not render (it printed `unsupported node kind: call`). The
// noreturn call must instead be emitted as an expression statement.
fn test_const_or_panic_tail_and_related_positions() {
	run_v3_const_or_panic_programs([
		ConstOrPanicCase{
			name:     'const_or_panic'
			source:   "fn decode(x int) !int {\n\tif x == 0 {\n\t\treturn error('zero')\n\t}\n\treturn x * 2\n}\n\nconst decoded = decode(3) or { panic('bad: \${err}') }\n\nfn main() {\n\tprintln(int_str(decoded))\n}\n"
			expected: '6'
		},
		ConstOrPanicCase{
			name:     'return_or_panic'
			source:   "fn decode(x int) !int {\n\tif x == 0 {\n\t\treturn error('zero')\n\t}\n\treturn x * 2\n}\n\nfn via(x int) int {\n\treturn decode(x) or { panic('bad: \${err}') }\n}\n\nfn main() {\n\tprintln(int_str(via(5)))\n}\n"
			expected: '10'
		},
		ConstOrPanicCase{
			name:     'value_or_default'
			source:   "fn decode(x int) !int {\n\tif x == 0 {\n\t\treturn error('zero')\n\t}\n\treturn x * 2\n}\n\nfn via(x int) int {\n\treturn decode(x) or { -1 }\n}\n\nfn main() {\n\tprintln(int_str(via(0)) + ':' + int_str(via(4)))\n}\n"
			expected: '-1:8'
		},
	])
}
