module fastc

import os
import v.cmdexec

fn test_fastc_nested_option_comparisons_and_narrowed_values() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_option_control_flow_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main

struct Counter {}

fn (counter Counter) maximum() ?(u64, string) {
	return u64(max_i32), max_i32.str()
}

fn maybe(value bool) ?bool {
	if !value {
		return none
	}
	return value
}

fn main() {
	assert (maybe(true) != none) != (maybe(false) != none)
	target := maybe(true)
	if target != none {
		copied := target
		assert copied
	} else {
		assert false
	}
	maximum, text := Counter{}.maximum() or { panic("missing maximum") }
	assert maximum == u64(2147483647)
	assert text == "2147483647"
	println("option flow works")
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
	assert run.output.trim_space() == 'option flow works', run.output
}
