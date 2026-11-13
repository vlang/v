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

// Heap storage must preserve value copies, equality and closure snapshots. Explicit
// pointers still compare by identity and their closure captures share the pointee.
fn test_fastc_heap_struct_locals_keep_value_semantics() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_heap_struct_values_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main
@[heap]
struct Box {
mut:
	value int
}
fn box_value(box Box) int {
	return box.value
}
fn bump_box(mut box Box) {
	box.value++
}
fn main() {
	mut original := Box{ value: 1 }
	equal := Box{ value: 1 }
	assert original == equal
	assert !(original != equal)
	assert &original != &equal
	parenthesized_copy := (original)
	copy_type := typeof(parenthesized_copy).name
	original_type := typeof(original).name
	assert copy_type == "Box"
	assert original_type == "Box"
	values := [original]
	values_type := typeof(values).name
	assert values_type == "[]Box"
	grouped_pointer := &(original)
	pointers := [&original]
	pointer_type := typeof(grouped_pointer).name
	pointers_type := typeof(pointers).name
	assert pointer_type == "&Box"
	assert pointers_type == "[]&Box"
	assert grouped_pointer == &original
	assert pointers[0] == &original
	unsafe {
		assert voidptr(original) == voidptr(&original)
	}
	assert (original).value == 1
	snapshot := fn [original] () int {
		return original.value
	}
	nested_snapshot := fn [original] () int {
		inner := fn [original] () int {
			return original.value
		}
		return inner()
	}
	mutable_snapshot := fn [mut original] () int {
		original.value++
		return original.value
	}
	rebound_snapshot := fn [mut original] () int {
		original = Box{ value: 7 }
		return original.value
	}
	mut pointer := &original
	pointer_snapshot := fn [pointer] () int {
		return pointer.value
	}
	mutable_pointer_snapshot := fn [mut pointer] () int {
		pointer.value++
		return pointer.value
	}
	assert box_value((original)) == 1
	bump_box(mut (original))
	assert original.value == 2
	original.value = 10
	assert parenthesized_copy.value == 1
	assert values[0].value == 1
	assert snapshot() == 1
	assert nested_snapshot() == 1
	assert mutable_snapshot() == 2
	assert mutable_snapshot() == 3
	assert rebound_snapshot() == 7
	assert original.value == 10
	assert grouped_pointer.value == 10
	assert pointers[0].value == 10
	assert pointer_snapshot() == 10
	assert mutable_pointer_snapshot() == 11
	assert original.value == 11
	assert snapshot() == 1
	assert &original == pointer
	assert pointer != &equal
	println("heap values preserved")
}
') or { panic(err) }
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, 'program_${backend}')
		build := cmdexec.run(@VEXE, ['-b', backend, '-nocache', '-o', program, root])
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'heap values preserved', run.output
	}
}
