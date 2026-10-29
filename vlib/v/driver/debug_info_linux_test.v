// vtest build: linux
module driver

import os

const debug_info_program = 'module main

@[noinline]
fn level_three(p &int) int {
	return unsafe { *p }
}

@[noinline]
fn level_two(p &int) int {
	return level_three(p) + 1
}

@[noinline]
fn level_one(p &int) int {
	return level_two(p) + 1
}

fn main() {
	p := unsafe { &int(nil) }
	println(level_one(p))
}
'

// `-g` must reach the C compiler and nothing may strip the result, so that a debugger and
// the crash handlers can name V functions. The section table and the symbol table of the
// binary are the evidence; running it under gdb is left to a human.
fn test_g_keeps_debug_information_and_symbols_with_gcc() {
	gcc := os.find_abs_path_of_executable('gcc') or { return }
	readelf := os.find_abs_path_of_executable('readelf') or { '' }
	nm := os.find_abs_path_of_executable('nm') or { '' }
	if readelf == '' && nm == '' {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'debug_info_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'crash.v')
	program := os.join_path(root, 'crash')
	os.write_file(source, debug_info_program)!
	build := os.exec([@VEXE, '-new-compiler', '-g', '-cc', gcc, '-o', program, source])
	assert build.exit_code == 0, build.output
	if readelf != '' {
		sections := os.exec([readelf, '-S', '--wide', program])
		assert sections.exit_code == 0, sections.output
		assert sections.output.contains('.debug_info'), 'no .debug_info in a `-g` build:\n${sections.output}'
	}
	if nm != '' {
		symbols := os.exec([nm, program])
		assert symbols.exit_code == 0, symbols.output
		for name in ['level_one', 'level_two', 'level_three'] {
			assert symbols.output.contains(' T ${name}\n') || symbols.output.contains(' t ${name}\n'), 'no symbol ${name} in a `-g` build'
		}
	}
}
