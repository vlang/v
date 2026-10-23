module c

import os

fn test_main_functions_compile_with_windows_header_macros() {
	cc := os.find_abs_path_of_executable('clang') or { return }
	dir := os.join_path(os.vtmp_dir(), 'windows_macro_functions_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'main.v')
	binary := os.join_path(dir, if os.user_os() == 'windows' { 'main.exe' } else { 'main' })
	os.write_file(source, 'fn near(a f64, b f64) bool { return a - b < 0.001 && b - a < 0.001 }
fn far(a int) int { return a + 1 }
fn pascal(a int) int { return a + 2 }
fn cdecl(a int) int { return a + 3 }
fn apply(callback fn (int) int, value int) int { return callback(value) }
fn apply_near(callback fn (f64, f64) bool) bool { return callback(1.0, 1.0005) }
fn main() {
	assert near(1.0, 1.0005)
	assert far(1) == 2
	assert pascal(1) == 3
	assert cdecl(1) == 4
	assert apply_near(near)
	assert apply(far, 2) == 3
	assert apply(pascal, 2) == 4
	assert apply(cdecl, 2) == 5
	println("ok")
}
')!
	compiled := os.exec([@VEXE, '-new-compiler', '-nocache', '-gc', 'none', '-cc', cc, '-cflags',
		'-Dnear= -Dfar= -Dpascal=__stdcall -Dcdecl=', '-o', binary, source])
	assert compiled.exit_code == 0, compiled.output
	ran := os.exec([binary])
	assert ran.exit_code == 0, ran.output
	assert ran.output.trim_space() == 'ok', ran.output
}
