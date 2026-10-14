module build

import os

const cycle_prog = "module main

import build

fn main() {
	mut ctx := build.context()
	ctx.task(name: 'a', depends: ['b'], run: fn (t build.Task) ! {
		println('a')
	})
	ctx.task(name: 'b', depends: ['a'], run: fn (t build.Task) ! {
		println('b')
	})
	ctx.exec('a')
}
"

fn test_indirect_cycle_is_reported() {
	// an indirect cycle (a -> b -> a) must report the cycle and exit,
	// not recurse until the stack overflows, so it runs in a child process
	dir := os.join_path(os.vtmp_dir(), 'build_cycle_test')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	prog := os.join_path(dir, 'cycle_prog.v')
	os.write_file(prog, cycle_prog)!
	result := os.exec([@VEXE, 'run', prog])
	assert result.exit_code == 1, result.output
	assert result.output.contains('cyclic task dependency detected'), result.output
	assert result.output.contains('a -> b -> a'), result.output
}
