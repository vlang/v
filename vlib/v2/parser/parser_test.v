module parser

import os
import time
import v2.ast
import v2.pref
import v2.token

fn parse_code(code string) ast.File {
	tmp_file := os.join_path(os.temp_dir(), 'v2_parser_test_${os.getpid()}_${time.now().unix_micro()}.v')
	os.write_file(tmp_file, code) or { panic(err) }
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &pref.Preferences{}
	mut file_set := token.FileSet.new()
	mut p := Parser.new(prefs)
	return p.parse_file(tmp_file, mut file_set)
}

fn first_array_init(code string) ast.ArrayInitExpr {
	file := parse_code(code)
	assert file.stmts.len == 1
	assert file.stmts[0] is ast.FnDecl
	fn_decl := file.stmts[0] as ast.FnDecl
	assert fn_decl.stmts.len == 1
	assert fn_decl.stmts[0] is ast.AssignStmt
	assign := fn_decl.stmts[0] as ast.AssignStmt
	assert assign.rhs.len == 1
	assert assign.rhs[0] is ast.ArrayInitExpr
	return assign.rhs[0] as ast.ArrayInitExpr
}

fn assert_anon_struct_array_type(typ ast.Expr, expect_fixed bool) {
	assert typ is ast.Type
	type_node := typ as ast.Type
	match type_node {
		ast.ArrayFixedType {
			assert expect_fixed
			assert type_node.elem_type is ast.Type
			assert (type_node.elem_type as ast.Type) is ast.AnonStructType
		}
		ast.ArrayType {
			assert !expect_fixed
			assert type_node.elem_type is ast.Type
			assert (type_node.elem_type as ast.Type) is ast.AnonStructType
		}
		else {
			assert false
		}
	}
}

fn test_dynamic_array_of_anon_struct_init() {
	array_init := first_array_init('fn main() { text_syms := []struct { name string value u64 }{} }')
	assert_anon_struct_array_type(array_init.typ, false)
}

fn test_fixed_array_of_anon_struct_init() {
	array_init := first_array_init('fn main() { points := [2]struct { x int y int }{} }')
	assert_anon_struct_array_type(array_init.typ, true)
}

fn test_return_stmt_allows_bare_prefix_exprs() {
	file := parse_code('fn ptr(v &int) &int { return &v }')
	assert file.stmts.len == 1
	assert file.stmts[0] is ast.FnDecl
	fn_decl := file.stmts[0] as ast.FnDecl
	assert fn_decl.stmts.len == 1
	assert fn_decl.stmts[0] is ast.ReturnStmt
	ret := fn_decl.stmts[0] as ast.ReturnStmt
	assert ret.exprs.len == 1
	assert ret.exprs[0] is ast.PrefixExpr
	prefix := ret.exprs[0] as ast.PrefixExpr
	assert prefix.op == .amp
	assert prefix.expr is ast.Ident
}

fn test_char_literal_assignment_parses() {
	file := parse_code('fn main() { c := `\\n` }')
	assert file.stmts.len == 1
	assert file.stmts[0] is ast.FnDecl
	fn_decl := file.stmts[0] as ast.FnDecl
	assert fn_decl.stmts.len == 1
	assert fn_decl.stmts[0] is ast.AssignStmt
	assign := fn_decl.stmts[0] as ast.AssignStmt
	assert assign.rhs.len == 1
	assert assign.rhs[0] is ast.BasicLiteral
	literal := assign.rhs[0] as ast.BasicLiteral
	assert literal.kind == .char
	assert literal.value == '\\n'
}

fn test_return_stmt_allows_amp_cast_expr_in_unsafe_method() {
	file := parse_code('module main

struct A {
	data voidptr
	element_size int
}

fn (a A) get_unsafe(i int) voidptr {
	unsafe {
		return &u8(a.data) + u64(i) * u64(a.element_size)
	}
}')
	assert file.stmts.len == 3
	assert file.stmts[2] is ast.FnDecl
}

fn test_struct_field_default_allows_amp_struct_init() {
	file := parse_code('module main

struct Foo {}

struct Bar {
	file &Foo = &Foo{}
}')
	assert file.stmts.len == 3
	assert file.stmts[2] is ast.StructDecl
}

fn test_reused_parser_resets_state_between_files() {
	tmp1 := os.join_path(os.temp_dir(), 'v2_parser_state_${os.getpid()}_${time.now().unix_micro()}_1.v')
	tmp2 := os.join_path(os.temp_dir(), 'v2_parser_state_${os.getpid()}_${time.now().unix_micro()}_2.v')
	os.write_file(tmp1, 'module main

fn main() {
	if true {}
}
') or { panic(err) }
	os.write_file(tmp2, 'module strings

// import rand
fn trim_spaces(s string) string {
	return s
}
') or {
		panic(err)
	}
	defer {
		os.rm(tmp1) or {}
		os.rm(tmp2) or {}
	}
	prefs := &pref.Preferences{}
	mut file_set := token.FileSet.new()
	mut parser := Parser.new(prefs)
	files := parser.parse_files([tmp1, tmp2], mut file_set)
	assert files.len == 2
	assert files[1].imports.len == 0
}

fn test_explicit_lifetime_generic_params_and_pointer_types_parse() {
	file := parse_code('module main

struct Ignore {}

struct IgnoreMatch[^a] {}

fn matched_dir_entry[^a](self &^a Ignore) IgnoreMatch[^a] {
	return IgnoreMatch[^a]{}
}
')
	assert file.stmts.len == 4
	assert file.stmts[2] is ast.StructDecl
	struct_decl := file.stmts[2] as ast.StructDecl
	assert struct_decl.generic_params.len == 1
	assert struct_decl.generic_params[0] is ast.LifetimeExpr
	lifetime_param := struct_decl.generic_params[0] as ast.LifetimeExpr
	assert lifetime_param.name == 'a'
	assert file.stmts[3] is ast.FnDecl
	fn_decl := file.stmts[3] as ast.FnDecl
	assert fn_decl.typ.generic_params.len == 1
	assert fn_decl.typ.generic_params[0] is ast.LifetimeExpr
	assert fn_decl.typ.params.len == 1
	assert fn_decl.typ.params[0].typ is ast.Type
	param_type := fn_decl.typ.params[0].typ as ast.Type
	assert param_type is ast.PointerType
	ptr_type := param_type as ast.PointerType
	assert ptr_type.lifetime == 'a'
	assert ptr_type.base_type is ast.Ident
	assert (ptr_type.base_type as ast.Ident).name == 'Ignore'
	assert fn_decl.typ.return_type is ast.Type
	return_type := fn_decl.typ.return_type as ast.Type
	assert return_type is ast.GenericType
	return_generic := return_type as ast.GenericType
	assert return_generic.name is ast.Ident
	assert (return_generic.name as ast.Ident).name == 'IgnoreMatch'
	assert return_generic.params.len == 1
	assert return_generic.params[0] is ast.LifetimeExpr
	assert (return_generic.params[0] as ast.LifetimeExpr).name == 'a'
}

fn test_explicit_lifetime_method_with_nested_generic_return_type_parse() {
	file := parse_code('module main

struct Ignore {}

struct DirEntry {}

struct Match[T] {
	value T
}

struct IgnoreMatch[^a] {
	ig &^a Ignore
}

fn (ig &^a Ignore) matched_dir_entry[^a](dent &DirEntry) Match[IgnoreMatch[^a]] {
	return Match[IgnoreMatch[^a]]{
		value: IgnoreMatch[^a]{
			ig: ig
		}
	}
}
')
	assert file.stmts.len == 6
	assert file.stmts[5] is ast.FnDecl
	fn_decl := file.stmts[5] as ast.FnDecl
	assert fn_decl.is_method
	assert fn_decl.receiver.typ is ast.Type
	receiver_type := fn_decl.receiver.typ as ast.Type
	assert receiver_type is ast.PointerType
	receiver_ptr := receiver_type as ast.PointerType
	assert receiver_ptr.lifetime == 'a'
	assert receiver_ptr.base_type is ast.Ident
	assert (receiver_ptr.base_type as ast.Ident).name == 'Ignore'
	assert fn_decl.typ.generic_params.len == 1
	assert fn_decl.typ.generic_params[0] is ast.LifetimeExpr
	assert (fn_decl.typ.generic_params[0] as ast.LifetimeExpr).name == 'a'
	assert fn_decl.typ.params.len == 1
	assert fn_decl.typ.params[0].typ is ast.Type
	dent_type := fn_decl.typ.params[0].typ as ast.Type
	assert dent_type is ast.PointerType
	dent_ptr := dent_type as ast.PointerType
	assert dent_ptr.lifetime == ''
	assert dent_ptr.base_type is ast.Ident
	assert (dent_ptr.base_type as ast.Ident).name == 'DirEntry'
	assert fn_decl.typ.return_type is ast.Type
	return_type := fn_decl.typ.return_type as ast.Type
	assert return_type is ast.GenericType
	outer_generic := return_type as ast.GenericType
	assert outer_generic.name is ast.Ident
	assert (outer_generic.name as ast.Ident).name == 'Match'
	assert outer_generic.params.len == 1
	assert outer_generic.params[0] is ast.Type
	inner_type := outer_generic.params[0] as ast.Type
	assert inner_type is ast.GenericType
	inner_generic := inner_type as ast.GenericType
	assert inner_generic.name is ast.Ident
	assert (inner_generic.name as ast.Ident).name == 'IgnoreMatch'
	assert inner_generic.params.len == 1
	assert inner_generic.params[0] is ast.LifetimeExpr
	assert (inner_generic.params[0] as ast.LifetimeExpr).name == 'a'
}
