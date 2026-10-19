module main

import os
import v.cmdexec

fn test_array_append_reuses_consumed_last_use_local_storage() {
	root := os.join_path(os.vtmp_dir(), 'array_append_last_use_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'main.v')
	c_path := os.join_path(root, 'main.c')
	os.write_file(path, '
struct AppendNode {
mut:
	children []AppendNode
	n int
}
struct AppendHolder {
	value AppendNode
}
fn make_append_node() AppendNode {
	return AppendNode{children: [AppendNode{n: 1}]}
}
fn consume_local(mut out []AppendNode) {
	value := make_append_node()
	out << value
}
fn consume_field(mut out []AppendNode) {
	value := AppendHolder{value: make_append_node()}
	out << value.value
}
fn append_tree(depth int) AppendNode {
	if depth == 0 {
		return AppendNode{n: 1}
	}
	left := append_tree(depth - 1)
	right := append_tree(depth - 1)
	mut children := []AppendNode{}
	children << left
	children << right
	return AppendNode{children: children}
}
fn retain_live_local(mut out []AppendNode) {
	value := make_append_node()
	out << value
	assert value.children.len == 1
}
fn retain_mutable_local(mut out []AppendNode) {
	mut value := make_append_node()
	out << value
	value.n = 2
}
fn retain_alias(mut out []AppendNode) {
	value := make_append_node()
	alias := value.children
	out << value
	assert alias.len == 1
}
fn retain_variable_origin(mut out []AppendNode, original AppendNode) {
	value := original
	out << value
}
fn retain_loop(mut out []AppendNode) {
	value := make_append_node()
	for _ in 0 .. 2 {
		out << value
	}
}
fn retain_twice(mut out []AppendNode) {
	value := AppendHolder{value: make_append_node()}
	out << value.value
	out << value.value
}
fn retain_defer(mut out []AppendNode) {
	value := make_append_node()
	defer { assert value.children.len == 1 }
	out << value
}
fn retain_capture(mut out []AppendNode) {
	value := make_append_node()
	read := fn [value] () int { return value.children.len }
	out << value
	assert read() == 1
}
fn append_node_identity(value AppendNode) AppendNode { return value }
fn retain_call_alias(mut out []AppendNode, original AppendNode) {
	value := append_node_identity(original)
	out << value
}
fn retain_literal_alias(mut out []AppendNode, original AppendNode) {
	value := AppendHolder{value: original}
	out << value.value
}
struct SharedAppend {
	left []int
	right []int
}
fn make_shared_append() SharedAppend {
	values := [1]
	return SharedAppend{left: values, right: values}
}
fn retain_shared_fields(mut out []SharedAppend) {
	value := make_shared_append()
	out << value
}
struct CustomAppend { n int }
fn (value CustomAppend) clone() CustomAppend { return CustomAppend{n: value.n + 10} }
fn make_custom_append() CustomAppend { return CustomAppend{n: 1} }
fn retain_custom_clone(mut out []CustomAppend) {
	value := make_custom_append()
	out << value
}
const shared_append = AppendNode{children: [AppendNode{n: 1}]}
fn make_scoped_const(flag bool) AppendNode {
	if flag {
		shared_append := 1
		if shared_append > 0 {
			return AppendNode{}
		}
	}
	return shared_append
}
fn retain_scoped_const(mut out []AppendNode) {
	value := make_scoped_const(false)
	out << value
}
fn wrapped_append_1() AppendNode { return wrapped_append_2() }
fn wrapped_append_2() AppendNode { return wrapped_append_3() }
fn wrapped_append_3() AppendNode { return shared_append }
fn retain_wrapped_const(mut out []AppendNode) {
	value := wrapped_append_1()
	out << value
}
fn main() {}
')!
	compiled := cmdexec.run(@VEXE, ['-new-compiler', '-no-skip-unused', '-o', c_path, path])
	assert compiled.exit_code == 0, compiled.output
	code := os.read_file(c_path)!
	for name in ['consume_local', 'consume_field'] {
		body := code.all_after_last('void ${name}(').all_after('{').all_before('\n}')
		assert body.contains('array_push('), body
		assert !body.contains('__derived_clone'), body
	}
	tree := code.all_after_last('main__AppendNode append_tree(').all_after('{').all_before('\n}')
	assert tree.contains('array_push('), tree
	assert !tree.contains('__derived_clone'), tree
	for name in ['retain_live_local', 'retain_mutable_local', 'retain_alias', 'retain_variable_origin',
		'retain_loop', 'retain_twice', 'retain_defer', 'retain_capture', 'retain_call_alias',
		'retain_literal_alias', 'retain_shared_fields', 'retain_scoped_const', 'retain_wrapped_const'] {
		body := code.all_after_last('void ${name}(').all_after('{').all_before('\n}')
		assert body.contains('__derived_clone'), '${name}: ${body}'
	}
	custom := code.all_after_last('void retain_custom_clone(').all_after('{').all_before('\n}')
	assert custom.contains('clone('), custom
}
