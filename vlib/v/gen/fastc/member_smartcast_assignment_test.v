module fastc

import os
import v.cmdexec
import v.pref

const member_smartcast_assignment_source = r'module main

type Val = int | string

struct Foo {
	x int
}

struct Inner {
mut:
	w Val
}

type Any = Foo | int

type Node = Inner | int

struct Box {
mut:
	v Val
	a Any
	n Node
}

fn set(mut b Box) {
	if mut b.v is int {
		b.v = -1
	}
}

fn set_string(mut b Box) {
	if mut b.v is int {
		b.v = "x"
	}
}

fn set_struct(mut b Box) {
	if mut b.a is Foo {
		b.a = Foo{
			x: b.a.x + 10
		}
	}
}

fn match_set(mut b Box) {
	match mut b.v {
		int {
			b.v = b.v * 10
		}
		string {
			b.v = b.v.len
		}
	}
}

fn nested_set(mut b Box) {
	if mut b.n is Inner {
		if b.n.w is int {
			b.n.w = b.n.w + 1
		}
	}
}

fn expr_set(mut b Box) int {
	return if mut b.v is int {
		b.v = 42
		1
	} else {
		0
	}
}

fn count_up(mut b Box) int {
	mut n := 0
	for b.v is int && b.v < 5 {
		n++
		b.v = b.v + 1
	}
	return n
}

fn show(b Box) string {
	v := match b.v {
		int { "int ${b.v}" }
		string { "string ${b.v}" }
	}
	a := match b.a {
		Foo { "Foo ${b.a.x}" }
		int { "int ${b.a}" }
	}
	w := match b.n {
		Inner {
			match b.n.w {
				int { "w ${b.n.w}" }
				string { "w ${b.n.w}" }
			}
		}
		int {
			"n ${b.n}"
		}
	}
	return v + " " + a + " " + w
}

fn main() {
	mut b := Box{
		v: 3
		a: Foo{
			x: 1
		}
		n: Inner{
			w: 5
		}
	}
	snapshot := b
	set(mut b)
	set_struct(mut b)
	nested_set(mut b)
	println(show(b))
	println(show(snapshot))
	set_string(mut b)
	println(show(b))
	match_set(mut b)
	println(show(b))
	match_set(mut b)
	println(show(b))
	println(expr_set(mut b))
	println(show(b))
	b.v = 1
	println(count_up(mut b))
	println(show(b))
}
'

// An assignment to a member narrowed by `is` or `match` (`if mut b.v is int { b.v = -1 }`)
// stores a new boxed value in the member, as cgen does. It used to reassign the C temporary
// that points at the old payload, so the member kept its value.
fn test_fastc_assignments_to_narrowed_members_store_in_the_member() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_member_smartcast_assignment_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), member_smartcast_assignment_source) or {
		panic(err)
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
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-pthread', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().split_into_lines() == ['int -1 Foo 11 w 6', 'int 3 Foo 1 w 6',
		'string x Foo 11 w 6', 'int 1 Foo 11 w 6', 'int 10 Foo 11 w 6', '1', 'int 42 Foo 11 w 6',
		'4', 'int 5 Foo 11 w 6'], run.output
}
