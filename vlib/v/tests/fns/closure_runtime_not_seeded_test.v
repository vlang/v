import os

// Import discovery seeds the closure runtime for syntax that might need it, and it scans
// the modules builtin imports too (strconv, strings, ...). A false positive there seeds
// the runtime into every program.
const vexe = os.quoted_path(@VEXE)

fn generated_c(name string, source string) string {
	root := os.join_path(os.vtmp_dir(), 'closure_runtime_not_seeded_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	path := os.join_path(root, '${name}.v')
	c_path := os.join_path(root, '${name}.c')
	os.write_file(path, source) or { panic(err) }
	res := os.exec([@VEXE, '-o', c_path, path])
	assert res.exit_code == 0, res.output
	return os.read_file(c_path) or { panic(err) }
}

fn testsuite_end() {
	os.rmdir_all(os.join_path(os.vtmp_dir(), 'closure_runtime_not_seeded_${os.getpid()}')) or {}
}

fn test_literal_output_program_has_no_closure_runtime() {
	c := generated_c('literal', "fn main() {\n\tprintln('Hello, World!')\n}\n")
	assert !c.contains('closure__'), 'the closure runtime was seeded'
}

fn test_program_without_closures_has_no_closure_runtime() {
	c := generated_c('plain', "fn main() {\n\tn := 3\n\tprintln('n: \${n}')\n}\n")
	assert !c.contains('closure__closure_init'), 'the closure runtime was seeded'
}

fn test_capturing_closure_keeps_closure_runtime() {
	c := generated_c('capturing',
		'fn main() {\n\tn := 3\n\tf := fn [n] () int {\n\t\treturn n\n\t}\n\tprintln(f())\n}\n')
	assert c.contains('closure__closure_init')
}
