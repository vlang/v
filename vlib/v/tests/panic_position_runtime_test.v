import os

const vexe = @VEXE
const vtmp_folder = os.join_path(os.vtmp_dir(), 'panic_position_runtime_tests_${os.getpid()}')

@[markused]
const turn_off_vcolors = os.setenv('VCOLORS', 'never', true)

const panic_program = "fn inner(x int) int {
	if x > 2 {
		panic('boom \${x}')
	}
	return x
}

fn middle(x int) int {
	return inner(x + 1)
}

fn main() {
	println(middle(2))
}
"

fn testsuite_begin() {
	os.mkdir_all(vtmp_folder) or {}
}

fn testsuite_end() {
	os.rmdir_all(vtmp_folder) or {}
}

// test_panic_shows_its_v_line_without_debug_info checks that a panic names the .v
// line of the `panic()` call, and the functions on the stack, without `-g`.
fn test_panic_shows_its_v_line_without_debug_info() {
	source_path, output := compile_and_run_panic_program('nodebug', [])
	lines := output.replace('\r', '').split_into_lines()
	assert lines[0] == 'V panic: boom 3', output
	assert lines[1] == '     at: ${source_path.replace('\\', '/')}:3 in main.inner()', output
	$if macos {
		// libSystem's backtrace() works for every C compiler, TinyCC included.
		frames := lines.filter(it.ends_with('()') && !it.contains(' '))
		assert frames == ['inner()', 'middle()', 'main()'], output
		assert output.contains('(build with `-g` to see the .v line of every frame)'), output
	}
}

// test_debug_panic_shows_the_v_line_of_every_frame checks the Go-like stack trace of a
// `-g` build: every V frame is followed by its .v file:line.
fn test_debug_panic_shows_the_v_line_of_every_frame() {
	$if !macos {
		// Other systems resolve frames with tools that are not always installed.
		return
	}
	if os.exec(['clang', '--version']).exit_code != 0 {
		return
	}
	source_path, output := compile_and_run_panic_program('debug', ['-cc', 'clang', '-g'])
	assert output.contains('     file: ${source_path}:3'), output
	real_source := os.real_path(source_path)
	assert output.contains('inner()\n\t${real_source}:3\nmiddle()\n\t${real_source}:9\nmain()\n\t${real_source}:13'), output
}

// test_print_backtrace_keeps_its_caller checks that `print_backtrace()` starts at the
// function that called it.
fn test_print_backtrace_keeps_its_caller() {
	$if !macos {
		return
	}
	source_path := os.join_path(vtmp_folder, 'print_backtrace.v')
	exe_path := os.join_path(vtmp_folder, 'print_backtrace${exe_suffix()}')
	os.write_file(source_path, 'fn show() {\n\tprint_backtrace()\n}\n\nfn main() {\n\tshow()\n}\n') or {
		panic(err)
	}
	compilation := os.exec([vexe, '-o', exe_path, source_path])
	assert compilation.exit_code == 0, 'compilation failed: ${compilation.output}'
	result := os.exec([exe_path])
	assert result.exit_code == 0, result.output
	frames := result.output.split_into_lines().filter(it.ends_with('()') && !it.contains(' '))
	assert frames == ['show()', 'main()'], result.output
}

// The panics are on lines 11, 19 and 27.
const generic_module_source = "module probe

pub fn ok() int {
	return 1
}

// fail panics for every value.
pub fn fail[T](x T) T {
	y := x
	if true {
		panic('probe fail')
	}
	return y
}

pub fn fail_spaced[T](x T) T {
	y := x
	if true {
		panic ('spaced')
	}
	return y
}

pub fn fail_commented[T](x T) T {
	y := x
	if true {
		panic/* why */('commented')
	}
	return y
}
"

const generic_module_second_source = 'module probe

pub fn length_of[T](value T) int {
	return value.len
}
'

// test_cached_generic_panic_keeps_its_source_position checks that a generic body,
// which a warm build reparses from the module cache header, still reports the line
// of its original source file.
fn test_cached_generic_panic_keeps_its_source_position() {
	$if windows {
		// The module cache needs a C compiler other than the bundled TinyCC.
		return
	}
	if os.exec(['cc', '--version']).exit_code != 0 {
		return
	}
	root := os.join_path(vtmp_folder, 'generic_module')
	module_dir := os.join_path(root, 'modules', 'probe')
	os.mkdir_all(module_dir) or { panic(err) }
	module_path := os.join_path(module_dir, 'a.v')
	os.write_file(module_path, generic_module_source) or { panic(err) }
	os.write_file(os.join_path(module_dir, 'b.v'), generic_module_second_source) or {
		panic(err)
	}
	old_vmodules := os.getenv('VMODULES')
	old_vtmp := os.getenv('VTMP')
	os.setenv('VMODULES', os.join_path(root, 'modules'), true)
	os.setenv('VTMP', os.join_path(root, 'vtmp'), true)
	defer {
		os.setenv('VMODULES', old_vmodules, true)
		os.setenv('VTMP', old_vtmp, true)
	}
	// The first build caches `probe`; the later ones specialize its functions again
	// from the cached header.
	cases := {
		'probe.fail[int](3)':                      11
		"probe.fail[string]('s')":                 11
		"probe.fail_spaced[string]('s')":          19
		'probe.fail_commented[f64](1.5)':          27
		"println(probe.length_of[string]('abc'))": 0
	}
	for i, call in cases.keys() {
		source_path := os.join_path(root, 'main_${i}.v')
		exe_path := os.join_path(root, 'main_${i}${exe_suffix()}')
		os.write_file(source_path, 'import probe\n\nfn main() {\n\tprintln(probe.ok())\n\t${call}\n}\n') or {
			panic(err)
		}
		compilation := os.exec([vexe, '-cc', 'cc', '-o', exe_path, source_path])
		assert compilation.exit_code == 0, 'compilation failed: ${compilation.output}'
		result := os.exec([exe_path])
		line := cases[call]
		if line == 0 {
			assert result.exit_code == 0, result.output
			continue
		}
		assert result.exit_code == 1, result.output
		assert result.output.contains('     at: ${module_path}:${line} in probe.fail'), result.output
	}
	// A declaration after a mapped one must not report the mapped one's file.
	source_path := os.join_path(root, 'main_error.v')
	os.write_file(source_path, 'import probe\n\nfn main() {\n\tprintln(probe.length_of[int](3))\n}\n') or {
		panic(err)
	}
	compilation := os.exec([vexe, '-cc', 'cc', '-o', os.join_path(root, 'main_error'), source_path])
	assert compilation.exit_code != 0, compilation.output
	assert compilation.output.contains('has no property `len`'), compilation.output
	assert !compilation.output.contains('a.v:'), compilation.output
}

fn compile_and_run_panic_program(name string, flags []string) (string, string) {
	source_path := os.join_path(vtmp_folder, '${name}.v')
	exe_path := os.join_path(vtmp_folder, '${name}${exe_suffix()}')
	os.write_file(source_path, panic_program) or { panic(err) }
	compilation := os.exec([vexe, ...flags, '-o', exe_path, source_path])
	assert compilation.exit_code == 0, 'compilation failed: ${compilation.output}'
	result := os.exec([exe_path])
	assert result.exit_code == 1, result.output
	return source_path, result.output
}

fn exe_suffix() string {
	return if os.user_os() == 'windows' { '.exe' } else { '' }
}
