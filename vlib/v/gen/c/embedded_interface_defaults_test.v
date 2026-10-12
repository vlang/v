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
	run_embedded_interface_defaults_source('nested', source, [])
}

fn test_log_use_stdout_preserves_embedded_defaults() {
	// Struct literals inherit Log.debug; use_stdout uses the configured constructor level.
	for level in ['info', 'debug'] {
		source := 'import log
fn main() {
	mut direct := log.ThreadSafeLog{}
	assert direct.get_level() == .debug
	log.use_stdout()
	assert log.get_level() == .${level}
	log.info("logger-defaults-ok")
}
'
		flags := if level == 'debug' { ['-d', 'log_default_level=debug'] } else { []string{} }
		output := run_embedded_interface_defaults_source('logger_${level}', source, flags)
		assert output.contains('logger-defaults-ok'), output
	}
}

fn run_embedded_interface_defaults_source(name string, source string, flags []string) string {
	root := os.join_path(os.vtmp_dir(), 'embedded_interface_defaults_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, source) or { panic(err) }
	mut args := ['-new-compiler', '-nocache']
	args << flags
	args << ['run', path]
	result := cmdexec.run(@VEXE, args)
	assert result.exit_code == 0, result.output
	return result.output
}
