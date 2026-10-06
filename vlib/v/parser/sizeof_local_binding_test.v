module parser

import os
import v.flat
import v.pref
import v.token

fn sizeof_local_parse(source string) &Parser {
	root := os.join_path(os.vtmp_dir(), 'sizeof_local_parse_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, source) or { panic(err) }
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	return p
}

fn test_sizeof_locals_and_parameters_do_not_index_module_declarations() {
	p := sizeof_local_parse('module main
fn measure(buf [3]u8) {
 _ = sizeof(buf)
 {
  local_buf := [8]int{}
  _ = sizeof(local_buf)
 }
 _ = sizeof(buf)
}
fn main() {
 values := [3]u8{}
 measure(values)
}
')
	assert p.translated_sizeof_scanned_modules.len == 0
	assert p.parsed_type_decls_end == 0
}

fn test_sizeof_local_shadows_module_constant_without_indexing_siblings() {
	p := sizeof_local_parse('module main
const buf = [1, 2, 3]!
fn main() {
 buf := [8]u8{}
 _ = sizeof(buf)
}
')
	assert p.translated_sizeof_scanned_modules.len == 0
	assert p.parsed_type_decls_end == 0
}

fn test_sizeof_shadowing_locals_and_parameters_keep_expression_operands() {
	p := sizeof_local_parse('module main
const buf = [1, 2, 3]!
const record = 1
struct Record {
 values [4]u16
}
fn measure(buf [8]u8) {
 _ = sizeof(buf)
 _ = sizeof(buf[0])
 _ = sizeof(buf[0] + u8(1))
}
fn main() {
 buf := [8]u8{}
 record := Record{}
 _ = sizeof(buf)
 _ = sizeof(buf[0])
 _ = sizeof(record.values)
 measure(buf)
}
')
	sizes := p.a.nodes.filter(it.kind == .sizeof_expr)
	assert sizes.len == 6
	for i, size in sizes {
		if i in [0, 3] {
			assert size.value == 'buf'
			assert size.children_count == 0
			continue
		}
		assert size.value == ''
		assert size.children_count == 1
		operand := p.a.node(p.a.child(&size, 0))
		assert operand.kind == [flat.NodeKind.ident, .index, .infix, .ident, .index, .selector][i]
	}
	assert p.translated_sizeof_scanned_modules.len == 0
	assert p.parsed_type_decls_end == 0
}

fn test_sizeof_module_constant_after_local_scope_still_indexes_declarations() {
	p := sizeof_local_parse('module main
const buf = [1, 2, 3]!
fn main() {
 {
  buf := [8]u8{}
  _ = sizeof(buf)
 }
 _ = sizeof(buf)
}
')
	assert p.translated_sizeof_scanned_modules.len > 0
	assert p.translated_sizeof_const_names[p.translated_sizeof_declaration_key('buf')]
}

fn test_sizeof_translated_local_and_constant_keep_existing_resolution() {
	p := sizeof_local_parse('@[translated]
module main
const buf = [1, 2, 3]!
fn main() {
 {
  buf := [8]u8{}
  _ = sizeof(buf)
 }
 _ = sizeof(buf)
}
')
	assert p.translated_sizeof_scanned_modules.len > 0
	assert p.translated_sizeof_const_names[p.translated_sizeof_declaration_key('buf')]
}

fn test_sizeof_local_type_alias_keeps_type_resolution_precedence() {
	mut p := Parser.new(pref.new_preferences())
	p.push_local_type_scope('scope')
	p.declare_local_type_name('buf', 'scope')
	p.begin_local_binding_scope()
	p.declare_local_binding('buf')
	assert !p.translated_sizeof_name_is_const('buf')
	assert p.translated_sizeof_scanned_modules.len == 0
	assert p.resolve_local_type_name('buf') != 'buf'
	source := 'sizeof(buf)'
	mut fs := token.FileSet.new()
	file := fs.add_file('sizeof_local_alias.v', source.len)
	p.s.init(file, source)
	p.next()
	id := p.sizeof_expr()
	size := p.a.node(id)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	assert size.kind == .sizeof_expr
	assert size.value == p.resolve_local_type_name('buf')
	assert size.children_count == 0
	assert p.translated_sizeof_scanned_modules.len == 0
}
