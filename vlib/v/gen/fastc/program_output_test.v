module fastc

import os
import v.cmdexec
import v.pref

// fastc_program_output translates `source` (a `module main` program) with the
// selfhost FastC path, compiles the C with `cc` and returns what the program
// printed to stdout and stderr, merged.
fn fastc_program_output(name string, source string) string {
	return fastc_program_output_files(name, {
		'main.v': source
	})
}

// fastc_program_output_files is `fastc_program_output` for a program made of
// several files; `files` maps paths relative to the program root (such as
// `mymod/mymod.v` for an imported module) to their source.
fn fastc_program_output_files(name string, files map[string]string) string {
	root := os.join_path(os.vtmp_dir(), 'fastc_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	for rel_path, source in files {
		path := os.join_path(root, rel_path)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		os.write_file(path, source) or { panic(err) }
	}
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

// The default `str()` keeps the module of a non-main struct (`mymod.Pt{`) and
// indents nested structs, arrays of structs and sum types like cgen does.
fn test_fastc_default_struct_str_keeps_module_and_indents_nested_values() {
	$if !macos && !linux {
		return
	}
	output := fastc_program_output_files('default_struct_str', {
		'main.v':        "module main

import mymod

struct Point {
	x int
	y int
}

struct Outer {
	p    Point
	name string
	arr  []Point
	mp   mymod.Pt
}

struct Deep {
	o Outer
}

type Val = int | Point

struct Holder {
	v Val
}

fn main() {
	println(mymod.Pt{1})
	println(mymod.Wrap{mymod.Pt{2}, [mymod.Pt{3}], 'w'})
	o := Outer{Point{1, 2}, 'n', [Point{3, 4}], mymod.Pt{7}}
	println(o)
	println(Deep{o})
	println(Holder{Val(Point{8, 9})})
	println('\${o.p}')
}
"
		'mymod/mymod.v': 'module mymod

pub struct Pt {
pub:
	x int
}

pub struct Wrap {
pub:
	p    Pt
	pts  []Pt
	name string
}
'
	})
	expected := [
		'mymod.Pt{',
		'    x: 1',
		'}',
		'mymod.Wrap{',
		'    p: mymod.Pt{',
		'        x: 2',
		'    }',
		'    pts: [mymod.Pt{',
		'        x: 3',
		'    }]',
		"    name: 'w'",
		'}',
		'Outer{',
		'    p: Point{',
		'        x: 1',
		'        y: 2',
		'    }',
		"    name: 'n'",
		'    arr: [Point{',
		'        x: 3',
		'        y: 4',
		'    }]',
		'    mp: mymod.Pt{',
		'        x: 7',
		'    }',
		'}',
		'Deep{',
		'    o: Outer{',
		'        p: Point{',
		'            x: 1',
		'            y: 2',
		'        }',
		"        name: 'n'",
		'        arr: [Point{',
		'            x: 3',
		'            y: 4',
		'        }]',
		'        mp: mymod.Pt{',
		'            x: 7',
		'        }',
		'    }',
		'}',
		'Holder{',
		'    v: Val(Point{',
		'        x: 8',
		'        y: 9',
		'    })',
		'}',
		'Point{',
		'    x: 1',
		'    y: 2',
		'}',
		'',
	].join('\n')
	assert output == expected, output
}

fn test_fastc_prints_pointers_and_options_like_cgen() {
	$if !macos && !linux {
		return
	}
	output := fastc_program_output('pointers_and_options', "struct Point {
	x int
	y int
}

enum Color {
	red
	green
}

struct Named {
	n int
}

fn (n Named) str() string {
	return 'N\${n.n}'
}

struct Holder {
	p  &Point
	np &Point
	nm &Named
	pc &Color
	pi &int
	op ?Point
	on ?int
	os ?string
}

fn show(mut p Point) {
	println(p)
	println('\${p}')
}

fn main() {
	p := &Point{1, 2}
	println(p)
	println('a \${p} b')
	nm := &Named{7}
	println(nm)
	println('\${nm}')
	a0 := [1, 2]
	arr := &a0
	println(arr)
	m := {
		'a': 1
	}
	mp := &m
	println('\${mp}')
	c := Color.green
	pc := &c
	println(pc)
	println('\${pc}')
	np := &Point(unsafe { nil })
	println(np)
	o := ?int(5)
	println(o)
	on := ?int(none)
	println('\${on}')
	op := ?Point(Point{3, 4})
	println(op)
	s := ?string('hi')
	println('\${s}')
	n := 7
	h := Holder{
		p:  p
		np: unsafe { nil }
		nm: nm
		pc: pc
		pi: &n
		op: Point{3, 4}
		os: 'x'
	}
	println(h)
	mut q := Point{5, 6}
	show(mut q)
}
")
	expected := [
		'&Point{',
		'    x: 1',
		'    y: 2',
		'}',
		'a &Point{',
		'    x: 1',
		'    y: 2',
		'} b',
		'&N7',
		'&N7',
		'&[1, 2]',
		"&{'a': 1}",
		'green',
		'green',
		'&nil',
		'Option(5)',
		'Option(none)',
		'Option(Point{',
		'    x: 3',
		'    y: 4',
		'})',
		"Option('hi')",
		'Holder{',
		'    p: &Point{',
		'        x: 1',
		'        y: 2',
		'    }',
		'    np: &nil',
		'    nm: &N7',
		'    pc: &green',
		'    pi: &7',
		'    op: Option(Point{',
		'        x: 3',
		'        y: 4',
		'    })',
		'    on: Option(none)',
		"    os: Option('x')",
		'}',
		'Point{',
		'    x: 5',
		'    y: 6',
		'}',
		'Point{',
		'    x: 5',
		'    y: 6',
		'}',
		'',
	].join('\n')
	assert output == expected, output
}
