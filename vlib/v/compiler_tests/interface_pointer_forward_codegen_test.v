// vtest ownership
import os
import v.cmdexec
import v.tempname

fn test_concrete_pointer_forwarded_to_interface_pointer_is_boxed_once() {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}
	root := os.join_path(os.vtmp_dir(), 'v3_interface_pointer_forward_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	project := os.join_path(root, 'v3_interface_pointer_forward_project')
	output := os.join_path(root, 'v3_interface_pointer_forward_program')
	os.mkdir_all(os.join_path(project, 'matcher')) or { panic(err) }
	os.mkdir_all(os.join_path(project, 'printer')) or { panic(err) }
	os.write_file(os.join_path(project, 'v.mod'), "Module { name: 'interface_pointer_forward' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(project, 'matcher', 'matcher.v'), 'module matcher

pub interface Finder {
	find_at(n int) !int
}

pub fn find_iter_at(finder &Finder, n int, found fn (int) bool) ! {
	value := finder.find_at(n)!
	_ = found(value)
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(project, 'printer', 'printer.v'), 'module printer

import matcher

pub struct Concrete {}

fn (c &Concrete) find_at(n int) !int {
	return n + 1
}

pub fn forward(finder &Concrete, n int) !int {
	matcher.find_iter_at(finder, n, fn (value int) bool {
		return value > 0
	})!
	return 42
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(project, 'main.v'), "module main

import printer

fn main() {
	finder := &printer.Concrete{}
	assert printer.forward(finder, 41) or { 0 } == 42
	println('ok')
}
") or {
		panic(err)
	}
	compile := cmdexec.run_in_merged_with_env(compiler, ['-ownership', '-d', 'ownership', '-nocache',
		'-no-parallel', '-o', output, os.join_path(project, 'main.v')], '', {
		'VFLAGS':               ''
		'VTOOLS_CACHE_VERBOSE': ''
	})
	assert compile.exit_code == 0, compile.output
	run := os.exec([output])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
