module parser

import os
import v.flat
import v.pref

fn test_child_buffer_preserves_nested_order_and_reuses_capacity() {
	path := os.join_path(os.vtmp_dir(), 'children_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	mut statements := []string{}
	for i in 0 .. 257 {
		statements << 'outer(inner(${i}), Item{value: inner(${i + 1})}, ' +
			"{'key': inner(${i + 2})})"
	}
	os.write_file(path, 'fn main() {\n' + statements.join('\n') + '\n}')!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.diagnostics.len == 0
	assert p.pending_children.len == 0
	capacity := p.pending_children.cap
	storage := p.pending_children.data
	assert capacity > 0
	mut count := 0
	for node in p.a.nodes {
		if node.kind != .call || p.a.child_node(&node, 0).value != 'outer' {
			continue
		}
		assert node.children_count == 4
		call := p.a.child_node(&node, 1)
		assert p.a.child_node(call, 1).value == count.str()
		fields := p.a.child_node(&node, 2)
		field := p.a.child_node(fields, 0)
		assert field.value == 'value'
		field_call := p.a.child_node(field, 0)
		assert p.a.child_node(field_call, 1).value == (count + 1).str()
		mapping := p.a.child_node(&node, 3)
		assert mapping.kind == flat.NodeKind.map_init
		assert p.a.child_node(mapping, 0).value == 'key'
		map_call := p.a.child_node(mapping, 1)
		assert p.a.child_node(map_call, 1).value == (count + 2).str()
		count++
	}
	assert count == statements.len
	p.parse_file(path)
	assert p.pending_children.len == 0
	assert p.pending_children.cap == capacity
	assert p.pending_children.data == storage
}

fn test_child_buffer_unwinds_incomplete_nested_input() {
	path := os.join_path(os.vtmp_dir(), 'children_error_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'fn main() { outer(inner(1), Item{value: inner(')!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.diagnostics.len > 0
	assert p.pending_children.len == 0
}

fn test_sizeof_known_types_does_not_index_sibling_declarations() {
	path := os.join_path(os.vtmp_dir(), 'sizeof_types_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	source := 'module main
struct Item { value int }
fn main() {
 _ = sizeof(int)
 _ = sizeof(string)
 _ = sizeof(Item)
 _ = sizeof(&Item)
 _ = sizeof([]Item)
 buffer := [8]u8{}
 _ = sizeof(buffer)
}
'
	os.write_file(path, source)!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.diagnostics.len == 0
	mut names := []string{}
	for node in p.a.nodes {
		if node.kind == .sizeof_expr {
			names << node.value
		}
	}
	expected := ['int', 'string', 'Item', '&Item', '[]Item', '']
	assert names == expected
	assert p.translated_sizeof_scanned_modules.len == 0
}
