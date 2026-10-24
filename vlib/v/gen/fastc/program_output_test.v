module fastc

import os
import v.cmdexec
import v.pref

// fastc_program_output translates `source` (a `module main` program) with the
// selfhost FastC path, compiles the C with `cc` and returns what the program
// printed to stdout and stderr.
fn fastc_program_output(name string, source string) string {
	root := os.join_path(os.vtmp_dir(), 'fastc_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	prefs.exclude = [os.join_path(@VEXEROOT, 'vlib/builtin/backtraces_nix.c.v')]
	c_source := generate_files([root], prefs) or { panic(err) }
	c_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-o', program, c_file, '-lm'])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	return run.output
}

// `typeof(x).name` spells types the way V does (`[]int`, `&Point`,
// `strings.Builder`, `[3]int`), and literal expressions as cgen does:
// `int literal` for integer ones, `f64` for float ones.
fn test_fastc_typeof_name_spells_types_like_v() {
	$if !macos && !linux {
		return
	}
	output := fastc_program_output('typeof_name', 'module main

import strings

struct Point {
	x int
}

fn main() {
	println(typeof(5).name)
	println(typeof(-5).name)
	println(typeof(2 + 3).name)
	println(typeof(-2 * 3).name)
	println(typeof(1.5).name)
	println(typeof(1.5 * 2).name)
	println(typeof(2 ** 3).name)
	println(typeof(`a`).name)
	a := [1, 2]
	println(typeof(a).name)
	m := {
		"a": 1
	}
	println(typeof(m).name)
	p := Point{1}
	println(typeof(p).name)
	pp := &p
	println(typeof(pp).name)
	b := strings.new_builder(1)
	println(typeof(b).name)
	f := [3]int{}
	println(typeof(f).name)
	ps := []&Point{}
	println(typeof(ps).name)
	mm := map[string][]Point{}
	println(typeof(mm).name)
	ff := [2][3]u8{}
	println(typeof(ff).name)
}
')
	expected := [
		'int literal',
		'int literal',
		'int literal',
		'int literal',
		'f64',
		'f64',
		'int literal',
		'rune',
		'[]int',
		'map[string]int',
		'Point',
		'&Point',
		'strings.Builder',
		'[3]int',
		'[]&Point',
		'map[string][]Point',
		'[2][3]u8',
		'',
	].join('\n')
	assert output == expected, output
}
