module fastc

import os
import v.cmdexec

fn test_fastc_selfhost_enum_context_and_imported_flag_receiver() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_flag_receiver_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'features')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), 'Module { name: "flag_receiver" }') or { panic(err) }
	os.write_file(os.join_path(root, 'features', 'features.v'), r'module features

@[flag]
pub enum Flags {
	read
	write
}

pub struct Counter {
pub mut:
	value int
}

pub fn select(mut calls Counter) Flags {
	calls.value++
	return .read
}

pub fn accepts(flag Flags) bool {
	return flag == .read
}

pub fn has_read(flag Flags) ?bool {
	return flag.has(.read)
}

pub fn assign(mut flag Flags) {
	mut count := 0
	count, flag = 1, .read
	assert count == 1
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r'module main

import features

fn main() {
	mut calls := features.Counter{}
	required := features.Flags.read
	excluded := features.Flags.write
	assert features.select(mut calls).has(required)
	assert features.select(mut calls).has(.read)
	assert !features.select(mut calls).has(excluded)
	assert calls.value == 3
	mut flag := features.Flags.write
	assert flag.has(excluded)
	mut count := 0
	count, flag = 7, .read
	assert count == 7
	assert flag == .read
	calls.value, flag = 8, .write
	assert calls.value == 8
	assert flag == .write
	flag, calls.value = .read, 9
	assert calls.value == 9
	assert flag == .read
	flag = .write
	features.assign(mut flag)
	assert flag == .read
	assert features.accepts(if count == 7 { .read } else { .write })
	assert !features.accepts(if count == 8 { .read } else { .write })
	assert features.has_read(required) or { false }
	assert !(features.has_read(excluded) or { true })
	println("flag receivers work")
}
') or { panic(err) }
	for backend in ['c', 'fastc'] {
		executable := os.join_path(root, 'program_${backend}')
		build := cmdexec.run(@VEXE, ['-b', backend, '-building-v', '-nocache', '-gc', 'none', '-o',
			executable, root])
		assert build.exit_code == 0, '${backend}: ${build.output}'
		run := cmdexec.run(executable, [])
		assert run.exit_code == 0, '${backend}: ${run.output}'
		assert run.output.trim_space() == 'flag receivers work', '${backend}: ${run.output}'
	}
}
