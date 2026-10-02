module c

import os
import v.cmdexec

fn test_embedded_struct_preserves_interface_and_scalar_defaults() {
	source := 'interface Value {
	value() int
}
struct Number {
	number int = 7
}
fn (n Number) value() int { return n.number }
struct Inner {
	payload Value = Number{}
	answer int = 42
}
struct Middle {
	Inner
}
struct Outer {
	Middle
}
fn main() {
	x := Outer{}
	assert x.answer == 42
	assert x.payload.value() == 7
	y := Outer{answer: 13}
	assert y.answer == 13
	assert y.payload.value() == 7
}
'
	run_embedded_interface_defaults_source('nested', source)
}

fn test_log_use_stdout_preserves_embedded_defaults() {
	source := 'import log
fn main() {
	mut direct := log.ThreadSafeLog{}
	assert direct.get_level() == .debug
	log.use_stdout()
	assert log.get_level() == .debug
	log.info("logger-defaults-ok")
}
'
	output := run_embedded_interface_defaults_source('logger', source)
	assert output.contains('logger-defaults-ok'), output
}

fn run_embedded_interface_defaults_source(name string, source string) string {
	root := os.join_path(os.vtmp_dir(), 'embedded_interface_defaults_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, source) or { panic(err) }
	result := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', 'run', path])
	assert result.exit_code == 0, result.output
	return result.output
}
