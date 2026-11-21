module fastc

import os
import v.cmdexec

// A bare `return` in a branch of an `if` expression leaves a `!` or `?` function with its
// success state.
fn test_fastc_bare_returns_in_expression_branches() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_bare_returns_in_expression_branches_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

fn mode(kind int) ! {
	ansi := if kind == 1 {
		true
	} else if kind == 2 {
		false
	} else {
		return
	}
	println(ansi)
}

fn maybe(kind int) ? {
	value := if kind > 0 { kind * 2 } else { return }
	println(value)
}

fn main() {
	mode(1)!
	mode(2)!
	mode(3)!
	maybe(4)?
	maybe(0)?
	println('done')
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
	assert run.output.trim_space() == 'true\nfalse\n8\ndone', run.output
}
