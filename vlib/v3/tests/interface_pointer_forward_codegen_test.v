import os

const interface_pointer_forward_vexe = @VEXE
const interface_pointer_forward_tests_dir = os.dir(@FILE)
const interface_pointer_forward_v3_dir = os.dir(interface_pointer_forward_tests_dir)
const interface_pointer_forward_vlib_dir = os.dir(interface_pointer_forward_v3_dir)
const interface_pointer_forward_v3_src = os.join_path(interface_pointer_forward_v3_dir, 'v3.v')

fn test_concrete_pointer_forwarded_to_interface_pointer_is_boxed_once() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_interface_pointer_forward_${pid}')
	project := os.join_path(os.temp_dir(), 'v3_interface_pointer_forward_project_${pid}')
	output := os.join_path(os.temp_dir(), 'v3_interface_pointer_forward_program_${pid}')
	defer {
		os.rm(v3_bin) or {}
		os.rmdir_all(project) or {}
		os.rm(output) or {}
		os.rm(output + '.c') or {}
	}
	build :=
		os.execute('${interface_pointer_forward_vexe} -gc none -d ownership -path "${interface_pointer_forward_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${interface_pointer_forward_v3_src}')
	assert build.exit_code == 0, build.output
	os.rmdir_all(project) or {}
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
	compile := os.execute('${v3_bin} -ownership -d ownership -nocache -no-parallel -o ${output} ${os.join_path(project,
		'main.v')}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(output)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
