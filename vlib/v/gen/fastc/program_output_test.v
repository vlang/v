module fastc

import os
import v.cmdexec
import v.pref

// fastc_program_output translates `source` (a `module main` program) with the
// selfhost FastC path, compiles the C with `cc` and returns what the program
// printed to stdout and stderr, merged.
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
	run := cmdexec.run_in_merged(program, [], root)
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

// `eprint`/`eprintln` print any value, as `print`/`println` do, to stderr.
fn test_fastc_eprint_prints_non_strings_to_stderr() {
	$if !macos && !linux {
		return
	}
	output := fastc_program_output('eprint', 'module main

enum Color {
	red
	green
}

struct Point {
	x int
}

fn main() {
	eprintln(5)
	eprintln(1.5)
	eprint(true)
	eprintln("")
	eprintln([1, 2])
	eprintln(Point{3})
	eprintln(Color.green)
	eprint(Color.red)
	eprintln("")
	println("stdout")
}
')
	expected := [
		'5',
		'1.5',
		'true',
		'[1, 2]',
		'Point{',
		'    x: 3',
		'}',
		'green',
		'red',
		'stdout',
		'',
	].join('\n')
	assert output == expected, output
}

// An inline map literal has the type of its first entry, so `println` prints it
// and an array literal of maps is typed as V types it.
fn test_fastc_println_prints_inline_map_literals() {
	$if !macos && !linux {
		return
	}
	output := fastc_program_output('inline_map', 'module main

fn count(m map[string]int) int {
	return m.len
}

fn main() {
	println({"k": 1})
	println({"a": "x", "b": "y"})
	print({1: 2.5})
	println("")
	println(count({"a": 1, "b": 2}))
	maps := [{"a": 1}, {"b": 2}]
	println(maps)
}
')
	expected := [
		"{'k': 1}",
		"{'a': 'x', 'b': 'y'}",
		'{1: 2.5}',
		'2',
		"[{'a': 1}, {'b': 2}]",
		'',
	].join('\n')
	assert output == expected, output
}

// Enum discriminants that use `**` on integer literals are folded to constants,
// grouping to the right as the C backend does.
fn test_fastc_enum_discriminants_fold_integer_powers() {
	$if !macos && !linux {
		return
	}
	output := fastc_program_output('enum_power', 'module main

enum Flag {
	a = 2 ** 3
	b
	c = 2 ** 2 ** 2
	d = 1 + 2 ** 4
	f = (2 ** 2) ** 3
}

fn main() {
	println(int(Flag.a))
	println(int(Flag.b))
	println(int(Flag.c))
	println(int(Flag.d))
	println(int(Flag.f))
	println(Flag.d)
}
')
	expected := ['8', '9', '16', '17', '64', 'd', ''].join('\n')
	assert output == expected, output
}
