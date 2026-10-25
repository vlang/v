module fastc

import os
import v.cmdexec

fn test_uint_preserves_platform_width_in_casts_interpolation_maps_and_boxes() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_platform_uint_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main

type Number = uint | string

fn identity(x uint) uint { return x }

fn unbox(x Number) uint {
	match x {
		uint { return x }
		string { return 0 }
	}
}

fn main() {
	assert sizeof(uint) == sizeof(voidptr)
	maximum := ~uint(0)
	assert maximum >> (sizeof(uint) * 8 - 1) == 1
	assert maximum + uint(1) == 0
	assert identity(maximum) == maximum
	assert unbox(Number(maximum)) == maximum
	mut m := map[uint]int{}
	m[maximum] = 17
	assert m[maximum] == 17
	$if x64 || arm64 {
		assert maximum.str() == "18446744073709551615"
		assert "\${maximum:x}" == "ffffffffffffffff"
		assert uint(1) << 40 == u64(1099511627776)
	} $else {
		assert maximum.str() == "4294967295"
		assert "\${maximum:x}" == "ffffffff"
	}
	println("ok")
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	built := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, root])
	assert built.exit_code == 0, built.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
