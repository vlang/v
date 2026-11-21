module fastc

import os
import v.cmdexec

// A `__global` of function type, alone or grouped, is a global and not the start of a
// function declaration.
fn test_fastc_globals_of_function_type() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_globals_of_function_type_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'hooks')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'hooks', 'hooks.v'), '@[has_globals]
module hooks

__global g_action fn (string) bool

__global (
	g_count   int
	g_handler fn (int) int
)

pub fn set_action(callback fn (string) bool) {
	g_action = callback
}

pub fn run(name string) bool {
	if isnil(g_action) {
		return false
	}
	return g_action(name)
}

pub fn set_handler(h fn (int) int) {
	g_handler = h
	g_count++
}

pub fn handle(x int) int {
	return g_handler(x) + g_count
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), "module main

import hooks

fn main() {
	println(hooks.run('a'))
	hooks.set_action(fn (name string) bool {
		return name == 'open'
	})
	println(hooks.run('open'))
	hooks.set_handler(fn (x int) int {
		return x * 2
	})
	println(hooks.handle(20))
}
") or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], os.real_path(os.join_path(@DIR, '../../../..')), {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'false\ntrue\n41', run.output
}
