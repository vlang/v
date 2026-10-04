module parser

import os
import v.pref

fn test_sizeof_reserved_types_do_not_index_sibling_declarations() {
	root := os.join_path(os.vtmp_dir(), 'sizeof_reserved_types_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	for name in ['bool', 'char', 'i8', 'i16', 'i32', 'int', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32',
		'f64', 'string', 'rune', 'usize', 'isize', 'voidptr'] {
		os.write_file(path, 'module main\nfn main() { _ = sizeof(${name}) }\n')!
		mut p := Parser.new(pref.new_preferences())
		p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		sizes := p.a.nodes.filter(it.kind == .sizeof_expr)
		assert sizes.len == 1
		assert sizes[0].value == name
		assert sizes[0].children_count == 0
		// These calls must not trigger a second scan of every file in their module.
		assert p.translated_sizeof_scanned_modules.len == 0
	}
}

fn test_sizeof_ordinary_constants_keep_expression_operands() {
	root := os.join_path(os.vtmp_dir(), 'sizeof_const_operands_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	// byte and array are valid const names, including when declared after use.
	for name in ['byte', 'array', 'charptr', 'i128', 'narrow'] {
		os.write_file(path, 'module main\nfn main() { _ = sizeof(${name}) }\nconst ${name} = u8(1)\n')!
		mut p := Parser.new(pref.new_preferences())
		p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		sizes := p.a.nodes.filter(it.kind == .sizeof_expr)
		assert sizes.len == 1
		assert sizes[0].value == ''
		assert sizes[0].children_count == 1
		operand := p.a.node(p.a.child(&sizes[0], 0))
		assert operand.kind == .ident
		assert operand.value == name
	}
}

fn test_sizeof_translated_local_type_name_keeps_expression_operand() {
	root := os.join_path(os.vtmp_dir(), 'sizeof_translated_operand_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, '@[translated]\nmodule main\nfn check(int string) { _ = sizeof(int) }\n')!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	sizes := p.a.nodes.filter(it.kind == .sizeof_expr)
	assert sizes.len == 1
	assert sizes[0].children_count == 1
	operand := p.a.node(p.a.child(&sizes[0], 0))
	assert operand.kind == .ident
	assert operand.value == 'int'
}
