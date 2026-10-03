module types

import os

const private_receiver_module = 'module counter

pub struct Counter {
mut:
 value int
}

pub fn (mut c Counter) inc() {
 c.value++
}

pub fn (c Counter) get() int {
 return c.value
}
'

fn check_private_receiver(name string, source string, run bool) os.Result {
	base := os.join_path(os.vtmp_dir(), 'private_mut_receiver_${name}_${os.getpid()}')
	os.mkdir_all(os.join_path(base, 'counter')) or { panic(err) }
	defer {
		os.rmdir_all(base) or {}
	}
	os.write_file(os.join_path(base, 'counter', 'counter.v'), private_receiver_module) or {
		panic(err)
	}
	file := os.join_path(base, 'main.v')
	os.write_file(file, 'import counter\n' + source) or { panic(err) }
	mode := if run { 'run' } else { '-check' }
	return os.exec([@VEXE, '-new-compiler', ...(os.split_args(mode) or { panic(err) }), file])
}

fn test_private_mut_method_rejects_immutable_local() {
	result := check_private_receiver('local', 'fn main() {
 c := counter.Counter{}
 c.inc()
}', false)
	assert result.exit_code != 0, result.output
	assert result.output.contains('`c` is immutable, declare it with `mut`'), result.output
}

fn test_private_mut_method_rejects_value_parameter() {
	result := check_private_receiver('parameter', 'fn start(c counter.Counter) {
 c.inc()
}
fn main() {
 mut c := counter.Counter{}
 start(c)
}', false)
	assert result.exit_code != 0, result.output
	assert result.output.contains('`c` is immutable, declare it with `mut`'), result.output
}

fn test_private_mut_method_rejects_immutable_loop_binding() {
	result := check_private_receiver('loop', 'fn main() {
 values := [counter.Counter{}]
 for c in values {
  c.inc()
 }
}', false)
	assert result.exit_code != 0, result.output
	assert result.output.contains('`c` is immutable, declare it with `mut`'), result.output
}

fn test_private_mut_method_rejects_immutable_value_receiver() {
	result := check_private_receiver('receiver', 'fn (c counter.Counter) start() {
 c.inc()
}
fn main() {
 mut c := counter.Counter{}
 c.start()
}', false)
	assert result.exit_code != 0, result.output
	assert result.output.contains('`c` is immutable, declare it with `mut`'), result.output
}

fn test_private_mut_method_preserves_mutable_parameter_state() {
	result := check_private_receiver('mutable', 'fn start(mut c counter.Counter) {
 c.inc()
}
fn main() {
 mut c := counter.Counter{}
 start(mut c)
 assert c.get() == 1
 c.inc()
 assert c.get() == 2
 println("ok")
}', true)
	assert result.exit_code == 0, result.output
	assert result.output.trim_space() == 'ok', result.output
}

fn test_private_mut_method_preserves_pointer_receiver_state() {
	result := check_private_receiver('pointer', 'fn main() {
 c := &counter.Counter{}
 c.inc()
 assert c.get() == 1
 println("ok")
}', true)
	assert result.exit_code == 0, result.output
	assert result.output.trim_space() == 'ok', result.output
}

fn test_os_command_start_rejects_immutable_value_receiver() {
	result := check_private_receiver('os_command', 'import os

fn (c os.Command) start_it() {
 c.start() or { panic(err) }
}
fn main() {
 mut cmd := os.Command{path: "echo hello"}
 cmd.start_it()
}', false)
	assert result.exit_code != 0, result.output
	assert result.output.contains('`c` is immutable, declare it with `mut`'), result.output
}
