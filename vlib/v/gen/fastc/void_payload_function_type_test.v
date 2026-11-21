module fastc

import os
import v.cmdexec

// Function types may spell a payload-free result or option as `!void` or `?void`.
fn test_fastc_function_types_accept_explicit_void_payloads() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_void_payload_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

type Callback = fn (int) ?void

struct Ops {
	add_fn fn (voidptr, u32, u32) !void = unsafe { nil }
	cb     Callback = unsafe { nil }
}

fn add(p voidptr, a u32, b u32) ! {
	if a == 0 {
		return error("zero")
	}
	println(a + b)
}

fn maybe(x int) ? {
	if x < 0 {
		return none
	}
	println(x)
}

fn call(f fn (int) ?void, x int) ? {
	f(x)?
}

fn main() {
	o := Ops{
		add_fn: add
		cb:     maybe
	}
	o.add_fn(unsafe { nil }, 1, 2) or { panic(err) }
	o.add_fn(unsafe { nil }, 0, 2) or { println(err.msg()) }
	o.cb(5) or { panic("none") }
	call(maybe, -1) or { println("none") }
	local := fn (x int) ! {
		println(x * 2)
	}
	local(21)!
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
	assert run.output.trim_space() == '3\nzero\n5\nnone\n42', run.output
}
