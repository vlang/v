module fastc

import os
import v.cmdexec

// Only parameterless `init` and `cleanup` functions are module lifecycle hooks.
// Like the C backend, FastC compiles the same names with parameters as ordinary
// functions that run only when called.
fn test_fastc_lifecycle_names_with_parameters_are_ordinary_functions() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_lifecycle_parameters_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'engine')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'engine', 'engine.v'), "module engine

pub struct Encoding {
pub:
	name string
}

__global loaded = 0

pub fn init(encodings []Encoding) !int {
	if encodings.len == 0 {
		return error('no encodings')
	}
	loaded += encodings.len
	return loaded
}

pub fn cleanup(code int) int {
	return code + loaded
}
") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

import engine

fn main() {
	total := engine.init([engine.Encoding{name: 'utf8'}, engine.Encoding{name: 'ascii'}]) or {
		panic(err)
	}
	engine.init([]engine.Encoding{}) or { println(err.msg()) }
	println('total ' + total.str())
	println('cleanup ' + engine.cleanup(10).str())
}
") or {
		panic(err)
	}
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
	assert run.output.trim_space() == 'no encodings\ntotal 2\ncleanup 12', run.output
}
