module fastc

import os
import v.cmdexec

// `T{}` with an enum `T` in a generic instance renders `(T){0}`: TinyCC rejects the empty
// initializer `(T){}` of a scalar.
fn test_fastc_empty_literal_of_enum_in_generic() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_empty_literal_of_enum_in_generic_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

enum Presentation {
	text
	emoji
}

struct Point {
	x int
	y int
}

fn parse_optional[T](current ?T) T {
	mut parsed := current or { T{} }
	return parsed
}

fn main() {
	println(parse_optional[Presentation](none))
	println(parse_optional[Presentation](Presentation.emoji))
	println(parse_optional[Point](none))
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
	assert run.output.trim_space() == 'text\nemoji\nPoint{\n    x: 0\n    y: 0\n}', run.output
}
