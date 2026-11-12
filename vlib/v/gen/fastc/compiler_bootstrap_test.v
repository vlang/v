module fastc

import os
import v.cmdexec

fn fastc_bootstrap_env(compiler string) map[string]string {
	return {
		'VFLAGS':                        ''
		'VOSARGS':                       ''
		'VEXE':                          compiler
		'V_MACOS_V3_NO_FALLBACK':        '1'
		'V_C_ERROR_BUG_REPORT_DISABLED': '1'
	}
}

fn fastc_bootstrap_run(compiler string, args []string, vroot string) os.Result {
	return cmdexec.run_in_merged_with_env(compiler, args, vroot, fastc_bootstrap_env(compiler))
}

fn test_fastc_full_compiler_bootstraps_three_generations() {
	$if !macos && !linux {
		return
	}
	vroot := os.real_path(os.join_path(@DIR, '../../../..'))
	root := os.join_path(os.vtmp_dir(), 'fastc_full_compiler_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_source := os.join_path(root, 'hello.v')
	os.write_file(c_source, 'module main

struct Value {
	amount int
}

type Scalar = Value | int | string

fn identity[T](value T) T {
	return value
}

fn lookup(values map[string]int, key string) ?int {
	return values[key] or { return none }
}

fn read_value(ok bool) !Scalar {
	if !ok {
		return error("no value")
	}
	return Value{amount: 42}
}

fn main() {
	values := {"answer": identity(42)}
	present := lookup(values, "answer") or { 0 }
	missing := lookup(values, "missing") or { -1 }
	assert present == 42
	assert missing == -1
	value := read_value(true) or { panic(err) }
	match value {
		Value { assert value.amount == 42 }
		int { assert false }
		string { assert false }
	}
	println("hello from c")
}
') or {
		panic(err)
	}
	fastc_source := os.join_path(root, 'fastc.v')
	os.write_file(fastc_source, 'module main
fn twice(value int) int { return value * 2 }
fn main() { println(twice(21)) }
') or { panic(err) }
	mut compiler := @VEXE
	for generation in 1 .. 4 {
		child := os.join_path(root, 'v_fastc_${generation}')
		// Compile the full CLI directly: `self -b fastc` selects the reduced
		// vlib/v/v.v driver and cannot prove the C backend survives this build.
		// The first generation covers `v -b fastc cmd/v` with the default GC
		// and cache settings; only the output path changes to isolate the test.
		mut build_args := ['-b', 'fastc']
		if generation > 1 {
			build_args << '-nocache'
		}
		build_args << ['-o', child, os.join_path(vroot, 'cmd', 'v')]
		build := fastc_bootstrap_run(compiler, build_args, vroot)
		assert build.exit_code == 0, 'generation ${generation}: ${build.output}'
		assert os.is_executable(child), 'generation ${generation}: missing compiler ${child}'
		compiler = child
		if generation == 1 {
			// A parallel check runs worker lanes in arenas of their own, which CI's
			// `VJOBS=1` skips; compile the compiler itself with the C backend on
			// several jobs once. A FastC-built compiler boxes sum payloads outside
			// those arenas and crashed here after the pass-2 lanes' arenas were
			// released (#30881).
			mut env := fastc_bootstrap_env(compiler)
			env['VJOBS'] = '4'
			cmd_v_c := os.join_path(root, 'cmd_v_${generation}.c')
			cmd_v_build := cmdexec.run_in_merged_with_env(compiler, ['-nocache', '-o', cmd_v_c,
				os.join_path(vroot, 'cmd', 'v')], vroot, env)
			assert cmd_v_build.exit_code == 0, 'parallel C build of cmd/v in generation ${generation}: ${cmd_v_build.output}'
			assert os.file_size(cmd_v_c) > 0, 'generation ${generation}: missing ${cmd_v_c}'
			os.rm(cmd_v_c) or {}
		}
		c_program := os.join_path(root, 'hello_${generation}')
		c_build := fastc_bootstrap_run(compiler, ['-b', 'c', '-nocache', '-o', c_program, c_source], vroot)
		assert c_build.exit_code == 0, 'C backend in generation ${generation}: ${c_build.output}'
		c_run := cmdexec.run(c_program, [])
		assert c_run.exit_code == 0, c_run.output
		assert c_run.output.trim_space() == 'hello from c', c_run.output
		fastc_program := os.join_path(root, 'fastc_${generation}')
		fastc_build := fastc_bootstrap_run(compiler, ['-b', 'fastc', '-nocache', '-o', fastc_program,
			fastc_source], vroot)
		assert fastc_build.exit_code == 0, 'FastC backend in generation ${generation}: ${fastc_build.output}'
		fastc_run := cmdexec.run(fastc_program, [])
		assert fastc_run.exit_code == 0, fastc_run.output
		assert fastc_run.output.trim_space() == '42', fastc_run.output
	}
}
