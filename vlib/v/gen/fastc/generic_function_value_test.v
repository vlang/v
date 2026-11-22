module fastc

import os
import v.cmdexec

// A generic function instance used as a value (`voidptr(entry[T])`) is specialized like a
// call.
fn test_fastc_generic_function_instances_as_values() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_generic_function_instances_as_values_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'lp')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'lp', 'lp.v'), "module lp

struct Args {
	n int
}

fn entry[T](ptr voidptr) voidptr {
	args := unsafe { &Args(ptr) }
	println('\${typeof[T]().name} \${args.n}')
	return unsafe { nil }
}

type EntryFn = fn (voidptr) voidptr

fn run(f voidptr, args voidptr) {
	call := unsafe { EntryFn(f) }
	call(args)
}

pub fn start[T](n int) {
	args := &Args{n}
	run(voidptr(entry[T]), voidptr(args))
}
") or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main

import lp

struct Event {}

fn main() {
	lp.start[Event](3)
	lp.start[int](4)
}
') or { panic(err) }
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
	assert run.output.trim_space() == 'Event 3\nint 4', run.output
}
