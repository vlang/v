module fastc

import os
import v.cmdexec

// `#flag -ffp-contract=off` is passed to the C compiler instead of being rejected.
fn test_fastc_accepts_fp_contract_flag() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_accepts_fp_contract_flag_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

#flag -ffp-contract=off

fn mul_add(a f64, b f64, c f64) f64 {
	return a * b + c
}

fn main() {
	println(mul_add(0.1, 0.2, 0.3))
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
	assert run.output.trim_space() == '0.32', run.output
}
