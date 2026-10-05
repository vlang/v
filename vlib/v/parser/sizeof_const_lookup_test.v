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

fn test_sizeof_types_of_the_module_do_not_index_sibling_declarations() {
	root := os.join_path(os.vtmp_dir(), 'sizeof_module_types_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	// Like `array` in `builtin`: a type that is not spelled as ordinary V types are.
	types_path := os.join_path(root, 'a_types.v')
	os.write_file(types_path, 'module main\nstruct lower_type {\n\tx int\n}\nfn first() { _ = sizeof(lower_type) }\n')!
	use_path := os.join_path(root, 'b_use.v')
	os.write_file(use_path, 'module main\nfn second() { _ = sizeof(lower_type) }\n')!
	mut p := Parser.new(pref.new_preferences())
	p.parse_files([types_path, use_path])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	sizes := p.a.nodes.filter(it.kind == .sizeof_expr)
	assert sizes.len == 2
	for size in sizes {
		assert size.value == 'lower_type'
		assert size.children_count == 0
	}
	// The declaration that the parser already read settles both without a second
	// scan of every file in the module.
	assert p.translated_sizeof_scanned_modules.len == 0
}

fn test_sizeof_types_of_another_module_do_not_hide_a_constant() {
	root := os.join_path(os.vtmp_dir(), 'sizeof_other_module_types_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'one'))!
	os.mkdir_all(os.join_path(root, 'two'))!
	defer { os.rmdir_all(root) or {} }
	type_path := os.join_path(root, 'one', 'one.v')
	os.write_file(type_path, 'module one\nstruct lower_type {\n\tx int\n}\n')!
	const_path := os.join_path(root, 'two', 'two.v')
	os.write_file(const_path, 'module two\nconst lower_type = u8(1)\nfn use() { _ = sizeof(lower_type) }\n')!
	// The same name in the same directory is another module's as well.
	sibling_path := os.join_path(root, 'two', 'other.v')
	os.write_file(sibling_path, 'module other\nstruct lower_type {\n\tx int\n}\n')!
	mut p := Parser.new(pref.new_preferences())
	p.parse_files([type_path, sibling_path, const_path])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	sizes := p.a.nodes.filter(it.kind == .sizeof_expr)
	assert sizes.len == 1
	assert sizes[0].value == ''
	assert sizes[0].children_count == 1
	operand := p.a.node(p.a.child(&sizes[0], 0))
	assert operand.kind == .ident
	assert operand.value == 'lower_type'
}

fn test_sizeof_of_a_builtin_type_does_not_index_builtin() {
	root := os.join_path(os.vtmp_dir(), 'sizeof_builtin_types_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'array.v')
	// `builtin` declares these names as types, so none of them is a const there,
	// and its directory, which every program parses, is not read once more for them.
	for name in ['array', 'mapnode', 'charptr', 'byteptr', 'i128', 'u128'] {
		os.write_file(path, 'module builtin\nfn check() { _ = sizeof(${name}) }\n')!
		mut p := Parser.new(pref.new_preferences())
		p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		sizes := p.a.nodes.filter(it.kind == .sizeof_expr)
		assert sizes.len == 1
		assert sizes[0].value == name
		assert sizes[0].children_count == 0
		assert p.translated_sizeof_scanned_modules.len == 0
	}
	// Another module can have a const of such a name, and is indexed for it.
	os.write_file(path, 'module main\nfn main() { _ = sizeof(array) }\n')!
	mut p := Parser.new(pref.new_preferences())
	p.parse_file(path)
	assert p.translated_sizeof_scanned_modules.len == 1
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
