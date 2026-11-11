module fastc

import os
import v.cmdexec

// A local of a `@[heap]` struct lives on the heap, as in the C backend, so a callee
// may keep `&local` or a `mut local` argument after the declaring frame returns.
fn test_fastc_heap_struct_locals_outlive_their_frame() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_heap_struct_local_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main
@[heap]
struct Ctrl {
mut:
	n     int
	name  string
	owner &Owner = unsafe { nil }
}
struct Owner {
mut:
	ctrl &Ctrl = unsafe { nil }
	all  []&Ctrl
}
fn (c Ctrl) describe() string {
	return "\${c.name}:\${c.n}"
}
fn (mut c Ctrl) bump() {
	c.n++
}
fn by_value(c Ctrl) int {
	return c.n
}
fn attach(mut o Owner, mut c Ctrl) {
	o.ctrl = &c
	c.owner = &o
}
fn make_value() Ctrl {
	c := Ctrl{ n: 7, name: "value" }
	return c
}
fn make_ref() &Ctrl {
	mut c := Ctrl{ n: 8 }
	c.bump()
	return &c
}
fn setup(mut o Owner) {
	mut c := Ctrl{ n: 42, name: "setup" }
	attach(mut o, mut c)
	c.bump()
	for i in 0 .. 3 {
		mut item := Ctrl{ n: i }
		o.all << &item
	}
}
@[noinline]
fn clobber() int {
	mut arr := [64]int{}
	for i in 0 .. arr.len { arr[i] = 0x400 + i }
	return arr[3]
}
fn main() {
	mut o := Owner{}
	setup(mut o)
	clobber()
	println(o.ctrl.n)
	println(o.ctrl.describe())
	println(o.ctrl.owner == &o)
	println(o.all.map(it.n))
	mut c := make_value()
	println(by_value(c))
	c = Ctrl{ n: 3, name: "re" }
	println(c.describe())
	r := make_ref()
	clobber()
	println(r.n)
	snapshot := c
	c.bump()
	println("\${snapshot.n} \${c.n}")
	f := fn [c] () int {
		return c.n
	}
	println(f())
	println([c, snapshot].map(it.n))
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-nocache', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '43\nsetup:43\ntrue\n[0, 1, 2]\n7\nre:3\n9\n3 4\n4\n[4, 3]', run.output
}
