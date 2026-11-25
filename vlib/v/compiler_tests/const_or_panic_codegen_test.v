import os
import v.tempname

fn run_v3_const_or_panic_program(name string, src string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_${name}_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	src_path := os.join_path(root, 'v3_${name}.v')
	bin_path := os.join_path(root, 'v3_${name}_bin')
	os.write_file(src_path, src) or { panic(err) }
	compile := os.exec([@VEXE, src_path, '-b', 'c', '-o', bin_path])
	assert compile.exit_code == 0, compile.output
	// The regressed path emitted `gen_node: unsupported node kind: call` to stderr
	// while still producing a binary, so assert the diagnostic is absent too.
	assert !compile.output.contains('unsupported node kind'), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin_path])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

// A const initialized by a Result-returning call guarded with `or { panic(..) }`
// used to reach the C backend as a bare `.call` node in the or-body tail, which
// gen_node could not render (it printed `unsupported node kind: call`). The
// noreturn call must instead be emitted as an expression statement.
fn test_const_or_panic_tail_and_related_positions() {
	const_or := run_v3_const_or_panic_program('const_or_panic',
		"fn decode(x int) !int {\n\tif x == 0 {\n\t\treturn error('zero')\n\t}\n\treturn x * 2\n}\n\nconst decoded = decode(3) or { panic('bad: \${err}') }\n\nfn main() {\n\tprintln(int_str(decoded))\n}\n")
	assert const_or == '6'

	return_or := run_v3_const_or_panic_program('return_or_panic',
		"fn decode(x int) !int {\n\tif x == 0 {\n\t\treturn error('zero')\n\t}\n\treturn x * 2\n}\n\nfn via(x int) int {\n\treturn decode(x) or { panic('bad: \${err}') }\n}\n\nfn main() {\n\tprintln(int_str(via(5)))\n}\n")
	assert return_or == '10'

	value_or := run_v3_const_or_panic_program('value_or_default',
		"fn decode(x int) !int {\n\tif x == 0 {\n\t\treturn error('zero')\n\t}\n\treturn x * 2\n}\n\nfn via(x int) int {\n\treturn decode(x) or { -1 }\n}\n\nfn main() {\n\tprintln(int_str(via(0)) + ':' + int_str(via(4)))\n}\n")
	assert value_or == '-1:8'
}
