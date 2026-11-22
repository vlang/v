module fastc

import os
import v.cmdexec

// V compares C structs whose fields it declares (`struct C.my_rect { x f64 }`), and aliases of
// them, field by field; C has no `==` for structs.
fn test_fastc_c_struct_equality_compares_fields() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_c_struct_equality_compares_fields_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

#include "@VMODROOT/rect.h"

@[typedef]
pub struct C.my_rect {
pub mut:
	x      f64
	y      f64
	width  f64
	height f64
}

type Rect = C.my_rect

fn frame(w f64) Rect {
	return Rect{
		width: w
	}
}

fn main() {
	old := frame(2)
	println(frame(2) != old)
	println(frame(3) != old)
	println(frame(2) == old)
	a := C.my_rect{
		x: 1
	}
	b := C.my_rect{
		x: 1
	}
	println(a == b)
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'rect.h'), 'typedef struct { double x; double y; double width; double height; } my_rect;\n') or { panic(err) }
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
	assert run.output.trim_space() == 'false\ntrue\ntrue\ntrue', run.output
}
