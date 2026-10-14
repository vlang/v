module fastc

import os
import v.cmdexec

fn fastc_assert_pointer_program(name string, source string, expected string) {
	root := os.join_path(os.vtmp_dir(), 'fastc_pointer_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	for backend in ['c', 'fastc'] {
		program := os.join_path(root, 'program_${backend}')
		build := cmdexec.run(@VEXE, ['-b', backend, '-gc', 'none', '-nocache', '-o', program, root])
		assert build.exit_code == 0, '${backend}: ${build.output}'
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, '${backend}: ${run.output}'
		assert run.output.trim_space() == expected, '${backend}: ${run.output}'
	}
}

fn test_fastc_local_fixed_arrays_preserve_pointer_elements() {
	$if !macos && !linux {
		return
	}
	fastc_assert_pointer_program('array', r'module main
struct Node {
mut:
	n int
}
fn use(n &Node) int {
	return n.n
}
fn first(mut nodes [2]&Node) int {
	return nodes[0].n
}
fn main() {
	mut p := &Node{n: 7}
	q := &Node{n: 9}
	mut a := [p, q]!
	p.n = 11
	println(use(a[0]))
	println(a[1].n)
	println(first(mut a))
}
', '11\n9\n11')
}

fn test_fastc_narrowed_interface_reference_compares_pointer_identity() {
	$if !macos && !linux {
		return
	}
	fastc_assert_pointer_program('interface', r'module main
interface INode {
	tag() int
}
struct Node {
	n int
}
fn (n Node) tag() int {
	return n.n
}
fn main() {
	p := &Node{n: 7}
	x := INode(p)
	if x is &Node && x == p {
		println("identity")
	}
}
', 'identity')
}

fn test_fastc_narrowed_interface_references_preserve_whole_branch_values() {
	$if !macos && !linux {
		return
	}
	fastc_assert_pointer_program('interface_body', r'module main
interface INode { tag() int }
struct Node { n int }
fn (n Node) tag() int { return n.n }
fn main() {
	p := &Node{n: 7}
	q := &Node{n: 9}
	x := INode(p)
	z := INode(q)
	if x is &Node && true { assert x == p; println(x.n) }
	if x is &Node && z is &Node { assert x == p; assert z == q; println(z.n) }
	y := if x is &Node && true { x } else { p }
	assert y == p
	println(y.n)
	w := if x is &Node && z is &Node { z } else { q }
	assert w == q
	println(w.n)
}
', '7\n9\n7\n9')
}
