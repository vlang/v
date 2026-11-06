module fastc

import os
import v.cmdexec

const pointer_member_param_source = 'module main

struct Foo {
	a int = 7
}

fn show(p &Foo) int {
	return p.a
}

fn main() {
	x := &Foo{}
	println(show(x))
}
'

const pointer_member_local_source = 'module main

struct Foo {
	a int = 7
}

fn main() {
	x := &Foo{}
	v := x.a
	println(v)
}
'

const pointer_member_value_source = 'module main

struct Foo {
	a int = 7
}

fn main() {
	mut z := Foo{}
	println(z.a)
}
'

// run_pointer_member_fastc_program compiles `source` with the fastc backend, runs it,
// and returns its stdout. A clang `.`/`->` mismatch surfaces as a build failure.
fn run_pointer_member_fastc_program(name string, source string) !string {
	root := os.join_path(os.vtmp_dir(), 'fastc_${name}_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), source)!
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], os.real_path(os.join_path(@DIR, '../../../..')), {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	if build.exit_code != 0 {
		return error('fastc build failed: ${build.output}')
	}
	run := cmdexec.run(program, [])
	if run.exit_code != 0 {
		return error('fastc program failed: ${run.output}')
	}
	return run.output
}

fn test_fastc_pointer_param_member_access_emits_arrow() {
	$if !macos && !linux {
		return
	}
	output := run_pointer_member_fastc_program('pointer_param', pointer_member_param_source) or {
		panic(err)
	}
	assert output.trim_space() == '7'
}

fn test_fastc_pointer_local_member_access_emits_arrow() {
	$if !macos && !linux {
		return
	}
	output := run_pointer_member_fastc_program('pointer_local', pointer_member_local_source) or {
		panic(err)
	}
	assert output.trim_space() == '7'
}

fn test_fastc_value_struct_member_access_keeps_dot() {
	$if !macos && !linux {
		return
	}
	output := run_pointer_member_fastc_program('value_local', pointer_member_value_source) or {
		panic(err)
	}
	assert output.trim_space() == '7'
}
