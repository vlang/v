// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast_dump

import strings
import v2.ast
import v2.token

// JsonBuilder provides JSON string building with proper escaping
struct JsonBuilder {
mut:
	sb     strings.Builder
	indent int
}

fn JsonBuilder.new() JsonBuilder {
	return JsonBuilder{
		sb: strings.new_builder(4096)
	}
}

fn (mut jb JsonBuilder) write_indent() {
	for _ in 0 .. jb.indent {
		jb.sb.write_string('  ')
	}
}

fn (mut jb JsonBuilder) write_string(s string) {
	jb.sb.write_string('"')
	for c in s {
		match c {
			`\\` { jb.sb.write_string('\\\\') }
			`"` { jb.sb.write_string('\\"') }
			`\n` { jb.sb.write_string('\\n') }
			`\r` { jb.sb.write_string('\\r') }
			`\t` { jb.sb.write_string('\\t') }
			else { jb.sb.write_u8(c) }
		}
	}
	jb.sb.write_string('"')
}

fn (mut jb JsonBuilder) str() string {
	return jb.sb.str()
}

// dump_files serializes an array of AST files to JSON
pub fn dump_files(files []ast.File) string {
	mut jb := JsonBuilder.new()
	jb.sb.write_string('[\n')
	jb.indent++
	for i, file in files {
		jb.write_file(file)
		if i < files.len - 1 {
			jb.sb.write_string(',')
		}
		jb.sb.write_string('\n')
	}
	jb.indent--
	jb.sb.write_string(']\n')
	return jb.str()
}

fn (mut jb JsonBuilder) write_file(file ast.File) {
	jb.write_indent()
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "File",\n')

	jb.write_indent()
	jb.sb.write_string('"name": ')
	jb.write_string(file.name)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"module": ')
	jb.write_string(file.mod)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"attributes": ')
	jb.write_attributes(file.attributes)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"imports": ')
	jb.write_imports(file.imports)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"stmts": ')
	jb.write_stmts(file.stmts)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_imports(imports []ast.ImportStmt) {
	jb.sb.write_string('[\n')
	jb.indent++
	for i, imp in imports {
		jb.write_import(imp)
		if i < imports.len - 1 {
			jb.sb.write_string(',')
		}
		jb.sb.write_string('\n')
	}
	jb.indent--
	jb.write_indent()
	jb.sb.write_string(']')
}

fn (mut jb JsonBuilder) write_import(imp ast.ImportStmt) {
	jb.write_indent()
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "ImportStmt",\n')

	jb.write_indent()
	jb.sb.write_string('"name": ')
	jb.write_string(imp.name)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"alias": ')
	jb.write_string(imp.alias)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"is_aliased": ${imp.is_aliased},\n')

	jb.write_indent()
	jb.sb.write_string('"symbols": ')
	jb.write_exprs(imp.symbols)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_attributes(attrs []ast.Attribute) {
	jb.sb.write_string('[\n')
	jb.indent++
	for i, attr in attrs {
		jb.write_attribute(attr)
		if i < attrs.len - 1 {
			jb.sb.write_string(',')
		}
		jb.sb.write_string('\n')
	}
	jb.indent--
	jb.write_indent()
	jb.sb.write_string(']')
}

fn (mut jb JsonBuilder) write_attribute(attr ast.Attribute) {
	jb.write_indent()
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"name": ')
	jb.write_string(attr.name)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"value": ')
	jb.write_expr(attr.value)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_stmts(stmts []ast.Stmt) {
	jb.sb.write_string('[\n')
	jb.indent++
	for i, stmt in stmts {
		jb.write_stmt(stmt)
		if i < stmts.len - 1 {
			jb.sb.write_string(',')
		}
		jb.sb.write_string('\n')
	}
	jb.indent--
	jb.write_indent()
	jb.sb.write_string(']')
}

fn (mut jb JsonBuilder) write_stmt(stmt ast.Stmt) {
	jb.write_indent()
	match stmt {
		ast.AssignStmt {
			jb.write_assign_stmt(stmt)
		}
		ast.AssertStmt {
			jb.write_assert_stmt(stmt)
		}
		ast.BlockStmt {
			jb.write_block_stmt(stmt)
		}
		ast.ConstDecl {
			jb.write_const_decl(stmt)
		}
		ast.DeferStmt {
			jb.write_defer_stmt(stmt)
		}
		ast.Directive {
			jb.write_directive(stmt)
		}
		ast.EnumDecl {
			jb.write_enum_decl(stmt)
		}
		ast.ExprStmt {
			jb.write_expr_stmt(stmt)
		}
		ast.FlowControlStmt {
			jb.write_flow_control_stmt(stmt)
		}
		ast.FnDecl {
			jb.write_fn_decl(stmt)
		}
		ast.ForStmt {
			jb.write_for_stmt(stmt)
		}
		ast.ForInStmt {
			jb.write_for_in_stmt(stmt)
		}
		ast.GlobalDecl {
			jb.write_global_decl(stmt)
		}
		ast.ImportStmt {
			jb.write_import(stmt)
		}
		ast.InterfaceDecl {
			jb.write_interface_decl(stmt)
		}
		ast.LabelStmt {
			jb.write_label_stmt(stmt)
		}
		ast.ModuleStmt {
			jb.write_module_stmt(stmt)
		}
		ast.ReturnStmt {
			jb.write_return_stmt(stmt)
		}
		ast.StructDecl {
			jb.write_struct_decl(stmt)
		}
		ast.TypeDecl {
			jb.write_type_decl(stmt)
		}
		ast.AsmStmt {
			jb.sb.write_string('{"type": "AsmStmt"}')
		}
		ast.ComptimeStmt {
			jb.write_comptime_stmt(stmt)
		}
		ast.EmptyStmt {
			jb.sb.write_string('{"type": "EmptyStmt"}')
		}
		[]ast.Attribute {
			jb.sb.write_string('{"type": "Attributes", "attrs": ')
			jb.write_attributes(stmt)
			jb.sb.write_string('}')
		}
	}
}

fn (mut jb JsonBuilder) write_assign_stmt(stmt ast.AssignStmt) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "AssignStmt",\n')

	jb.write_indent()
	jb.sb.write_string('"op": ')
	jb.write_string(stmt.op.str())
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"lhs": ')
	jb.write_exprs(stmt.lhs)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"rhs": ')
	jb.write_exprs(stmt.rhs)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"pos": ')
	jb.write_pos(stmt.pos)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_assert_stmt(stmt ast.AssertStmt) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "AssertStmt",\n')

	jb.write_indent()
	jb.sb.write_string('"expr": ')
	jb.write_expr(stmt.expr)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"extra": ')
	jb.write_expr(stmt.extra)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_block_stmt(stmt ast.BlockStmt) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "BlockStmt",\n')

	jb.write_indent()
	jb.sb.write_string('"stmts": ')
	jb.write_stmts(stmt.stmts)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_const_decl(stmt ast.ConstDecl) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "ConstDecl",\n')

	jb.write_indent()
	jb.sb.write_string('"is_public": ${stmt.is_public},\n')

	jb.write_indent()
	jb.sb.write_string('"fields": ')
	jb.write_field_inits(stmt.fields)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_defer_stmt(stmt ast.DeferStmt) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "DeferStmt",\n')

	jb.write_indent()
	jb.sb.write_string('"mode": ')
	jb.write_string(stmt.mode.str())
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"stmts": ')
	jb.write_stmts(stmt.stmts)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_directive(stmt ast.Directive) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "Directive",\n')

	jb.write_indent()
	jb.sb.write_string('"name": ')
	jb.write_string(stmt.name)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"value": ')
	jb.write_string(stmt.value)
	if stmt.ct_cond.len > 0 {
		jb.sb.write_string(',\n')
		jb.write_indent()
		jb.sb.write_string('"ct_cond": ')
		jb.write_string(stmt.ct_cond)
	}
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_enum_decl(stmt ast.EnumDecl) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "EnumDecl",\n')

	jb.write_indent()
	jb.sb.write_string('"name": ')
	jb.write_string(stmt.name)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"is_public": ${stmt.is_public},\n')

	jb.write_indent()
	jb.sb.write_string('"attributes": ')
	jb.write_attributes(stmt.attributes)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"as_type": ')
	jb.write_expr(stmt.as_type)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"fields": ')
	jb.write_field_decls(stmt.fields)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_expr_stmt(stmt ast.ExprStmt) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "ExprStmt",\n')

	jb.write_indent()
	jb.sb.write_string('"expr": ')
	jb.write_expr(stmt.expr)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_flow_control_stmt(stmt ast.FlowControlStmt) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "FlowControlStmt",\n')

	jb.write_indent()
	jb.sb.write_string('"op": ')
	jb.write_string(stmt.op.str())
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"label": ')
	jb.write_string(stmt.label)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_fn_decl(stmt ast.FnDecl) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "FnDecl",\n')

	jb.write_indent()
	jb.sb.write_string('"name": ')
	jb.write_string(stmt.name)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"is_public": ${stmt.is_public},\n')

	jb.write_indent()
	jb.sb.write_string('"is_method": ${stmt.is_method},\n')

	jb.write_indent()
	jb.sb.write_string('"is_static": ${stmt.is_static},\n')

	jb.write_indent()
	jb.sb.write_string('"language": ')
	jb.write_string(stmt.language.str())
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"attributes": ')
	jb.write_attributes(stmt.attributes)
	jb.sb.write_string(',\n')

	if stmt.is_method {
		jb.write_indent()
		jb.sb.write_string('"receiver": ')
		jb.write_parameter(stmt.receiver)
		jb.sb.write_string(',\n')
	}

	jb.write_indent()
	jb.sb.write_string('"fn_type": ')
	jb.write_fn_type(stmt.typ)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"stmts": ')
	jb.write_stmts(stmt.stmts)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"pos": ')
	jb.write_pos(stmt.pos)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_for_stmt(stmt ast.ForStmt) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "ForStmt",\n')

	jb.write_indent()
	jb.sb.write_string('"init": ')
	jb.write_stmt(stmt.init)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"cond": ')
	jb.write_expr(stmt.cond)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"post": ')
	jb.write_stmt(stmt.post)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"stmts": ')
	jb.write_stmts(stmt.stmts)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_for_in_stmt(stmt ast.ForInStmt) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "ForInStmt",\n')

	jb.write_indent()
	jb.sb.write_string('"key": ')
	jb.write_expr(stmt.key)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"value": ')
	jb.write_expr(stmt.value)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"expr": ')
	jb.write_expr(stmt.expr)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_global_decl(stmt ast.GlobalDecl) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "GlobalDecl",\n')

	jb.write_indent()
	jb.sb.write_string('"attributes": ')
	jb.write_attributes(stmt.attributes)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"fields": ')
	jb.write_field_decls(stmt.fields)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_interface_decl(stmt ast.InterfaceDecl) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "InterfaceDecl",\n')

	jb.write_indent()
	jb.sb.write_string('"name": ')
	jb.write_string(stmt.name)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"is_public": ${stmt.is_public},\n')

	jb.write_indent()
	jb.sb.write_string('"attributes": ')
	jb.write_attributes(stmt.attributes)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"generic_params": ')
	jb.write_exprs(stmt.generic_params)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"embedded": ')
	jb.write_exprs(stmt.embedded)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"fields": ')
	jb.write_field_decls(stmt.fields)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_label_stmt(stmt ast.LabelStmt) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "LabelStmt",\n')

	jb.write_indent()
	jb.sb.write_string('"name": ')
	jb.write_string(stmt.name)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"stmt": ')
	jb.write_stmt(stmt.stmt)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_module_stmt(stmt ast.ModuleStmt) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "ModuleStmt",\n')

	jb.write_indent()
	jb.sb.write_string('"name": ')
	jb.write_string(stmt.name)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_return_stmt(stmt ast.ReturnStmt) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "ReturnStmt",\n')

	jb.write_indent()
	jb.sb.write_string('"exprs": ')
	jb.write_exprs(stmt.exprs)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_struct_decl(stmt ast.StructDecl) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "StructDecl",\n')

	jb.write_indent()
	jb.sb.write_string('"name": ')
	jb.write_string(stmt.name)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"is_public": ${stmt.is_public},\n')

	jb.write_indent()
	jb.sb.write_string('"is_union": ${stmt.is_union},\n')

	jb.write_indent()
	jb.sb.write_string('"language": ')
	jb.write_string(stmt.language.str())
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"attributes": ')
	jb.write_attributes(stmt.attributes)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"generic_params": ')
	jb.write_exprs(stmt.generic_params)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"embedded": ')
	jb.write_exprs(stmt.embedded)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"fields": ')
	jb.write_field_decls(stmt.fields)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"pos": ')
	jb.write_pos(stmt.pos)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_type_decl(stmt ast.TypeDecl) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "TypeDecl",\n')

	jb.write_indent()
	jb.sb.write_string('"name": ')
	jb.write_string(stmt.name)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"is_public": ${stmt.is_public},\n')

	jb.write_indent()
	jb.sb.write_string('"language": ')
	jb.write_string(stmt.language.str())
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"generic_params": ')
	jb.write_exprs(stmt.generic_params)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"base_type": ')
	jb.write_expr(stmt.base_type)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"variants": ')
	jb.write_exprs(stmt.variants)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_comptime_stmt(stmt ast.ComptimeStmt) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "ComptimeStmt",\n')

	jb.write_indent()
	jb.sb.write_string('"stmt": ')
	jb.write_stmt(stmt.stmt)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_exprs(exprs []ast.Expr) {
	jb.sb.write_string('[\n')
	jb.indent++
	for i, expr in exprs {
		jb.write_indent()
		jb.write_expr(expr)
		if i < exprs.len - 1 {
			jb.sb.write_string(',')
		}
		jb.sb.write_string('\n')
	}
	jb.indent--
	jb.write_indent()
	jb.sb.write_string(']')
}

fn (mut jb JsonBuilder) write_expr(expr ast.Expr) {
	match expr {
		ast.ArrayInitExpr {
			jb.write_array_init_expr(expr)
		}
		ast.AsCastExpr {
			jb.write_as_cast_expr(expr)
		}
		ast.AssocExpr {
			jb.write_assoc_expr(expr)
		}
		ast.BasicLiteral {
			jb.write_basic_literal(expr)
		}
		ast.CallExpr {
			jb.write_call_expr(expr)
		}
		ast.CallOrCastExpr {
			jb.write_call_or_cast_expr(expr)
		}
		ast.CastExpr {
			jb.write_cast_expr(expr)
		}
		ast.ComptimeExpr {
			jb.write_comptime_expr(expr)
		}
		ast.EmptyExpr {
			jb.sb.write_string('{"type": "EmptyExpr"}')
		}
		ast.FieldInit {
			jb.write_field_init(expr)
		}
		ast.FnLiteral {
			jb.write_fn_literal(expr)
		}
		ast.GenericArgOrIndexExpr {
			jb.write_generic_arg_or_index_expr(expr)
		}
		ast.GenericArgs {
			jb.write_generic_args(expr)
		}
		ast.Ident {
			jb.write_ident(expr)
		}
		ast.IfExpr {
			jb.write_if_expr(expr)
		}
		ast.IfGuardExpr {
			jb.write_if_guard_expr(expr)
		}
		ast.IndexExpr {
			jb.write_index_expr(expr)
		}
		ast.InfixExpr {
			jb.write_infix_expr(expr)
		}
		ast.InitExpr {
			jb.write_init_expr(expr)
		}
		ast.Keyword {
			jb.write_keyword(expr)
		}
		ast.KeywordOperator {
			jb.write_keyword_operator(expr)
		}
		ast.LambdaExpr {
			jb.write_lambda_expr(expr)
		}
		ast.LockExpr {
			jb.write_lock_expr(expr)
		}
		ast.MapInitExpr {
			jb.write_map_init_expr(expr)
		}
		ast.MatchExpr {
			jb.write_match_expr(expr)
		}
		ast.ModifierExpr {
			jb.write_modifier_expr(expr)
		}
		ast.OrExpr {
			jb.write_or_expr(expr)
		}
		ast.ParenExpr {
			jb.write_paren_expr(expr)
		}
		ast.PostfixExpr {
			jb.write_postfix_expr(expr)
		}
		ast.PrefixExpr {
			jb.write_prefix_expr(expr)
		}
		ast.RangeExpr {
			jb.write_range_expr(expr)
		}
		ast.SelectExpr {
			jb.write_select_expr(expr)
		}
		ast.SelectorExpr {
			jb.write_selector_expr(expr)
		}
		ast.SqlExpr {
			jb.sb.write_string('{"type": "SqlExpr"}')
		}
		ast.StringInterLiteral {
			jb.write_string_inter_literal(expr)
		}
		ast.StringLiteral {
			jb.write_string_literal(expr)
		}
		ast.Tuple {
			jb.write_tuple(expr)
		}
		ast.Type {
			jb.write_type_expr(expr)
		}
		ast.UnsafeExpr {
			jb.write_unsafe_expr(expr)
		}
	}
}

fn (mut jb JsonBuilder) write_array_init_expr(expr ast.ArrayInitExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "ArrayInitExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"elem_type": ')
	jb.write_expr(expr.typ)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"exprs": ')
	jb.write_exprs(expr.exprs)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"init": ')
	jb.write_expr(expr.init)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"cap": ')
	jb.write_expr(expr.cap)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"len": ')
	jb.write_expr(expr.len)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"pos": ')
	jb.write_pos(expr.pos)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_as_cast_expr(expr ast.AsCastExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "AsCastExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"expr": ')
	jb.write_expr(expr.expr)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"cast_type": ')
	jb.write_expr(expr.typ)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"pos": ')
	jb.write_pos(expr.pos)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_assoc_expr(expr ast.AssocExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "AssocExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"assoc_type": ')
	jb.write_expr(expr.typ)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"expr": ')
	jb.write_expr(expr.expr)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"fields": ')
	jb.write_field_inits(expr.fields)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_basic_literal(expr ast.BasicLiteral) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "BasicLiteral",\n')

	jb.write_indent()
	jb.sb.write_string('"kind": ')
	jb.write_string(expr.kind.str())
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"value": ')
	jb.write_string(expr.value)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_call_expr(expr ast.CallExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "CallExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"lhs": ')
	jb.write_expr(expr.lhs)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"args": ')
	jb.write_exprs(expr.args)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"pos": ')
	jb.write_pos(expr.pos)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_call_or_cast_expr(expr ast.CallOrCastExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "CallOrCastExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"lhs": ')
	jb.write_expr(expr.lhs)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"expr": ')
	jb.write_expr(expr.expr)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"pos": ')
	jb.write_pos(expr.pos)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_cast_expr(expr ast.CastExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "CastExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"cast_type": ')
	jb.write_expr(expr.typ)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"expr": ')
	jb.write_expr(expr.expr)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"pos": ')
	jb.write_pos(expr.pos)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_comptime_expr(expr ast.ComptimeExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "ComptimeExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"expr": ')
	jb.write_expr(expr.expr)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"pos": ')
	jb.write_pos(expr.pos)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_field_init(expr ast.FieldInit) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "FieldInit",\n')

	jb.write_indent()
	jb.sb.write_string('"name": ')
	jb.write_string(expr.name)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"value": ')
	jb.write_expr(expr.value)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_field_inits(fields []ast.FieldInit) {
	jb.sb.write_string('[\n')
	jb.indent++
	for i, field in fields {
		jb.write_indent()
		jb.write_field_init(field)
		if i < fields.len - 1 {
			jb.sb.write_string(',')
		}
		jb.sb.write_string('\n')
	}
	jb.indent--
	jb.write_indent()
	jb.sb.write_string(']')
}

fn (mut jb JsonBuilder) write_field_decls(fields []ast.FieldDecl) {
	jb.sb.write_string('[\n')
	jb.indent++
	for i, field in fields {
		jb.write_indent()
		jb.write_field_decl(field)
		if i < fields.len - 1 {
			jb.sb.write_string(',')
		}
		jb.sb.write_string('\n')
	}
	jb.indent--
	jb.write_indent()
	jb.sb.write_string(']')
}

fn (mut jb JsonBuilder) write_field_decl(field ast.FieldDecl) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"name": ')
	jb.write_string(field.name)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"field_type": ')
	jb.write_expr(field.typ)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"value": ')
	jb.write_expr(field.value)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"attributes": ')
	jb.write_attributes(field.attributes)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_fn_literal(expr ast.FnLiteral) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "FnLiteral",\n')

	jb.write_indent()
	jb.sb.write_string('"fn_type": ')
	jb.write_fn_type(expr.typ)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"captured_vars": ')
	jb.write_exprs(expr.captured_vars)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"stmts": ')
	jb.write_stmts(expr.stmts)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_generic_arg_or_index_expr(expr ast.GenericArgOrIndexExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "GenericArgOrIndexExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"lhs": ')
	jb.write_expr(expr.lhs)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"expr": ')
	jb.write_expr(expr.expr)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_generic_args(expr ast.GenericArgs) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "GenericArgs",\n')

	jb.write_indent()
	jb.sb.write_string('"lhs": ')
	jb.write_expr(expr.lhs)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"args": ')
	jb.write_exprs(expr.args)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_ident(expr ast.Ident) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "Ident",\n')

	jb.write_indent()
	jb.sb.write_string('"name": ')
	jb.write_string(expr.name)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"pos": ')
	jb.write_pos(expr.pos)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_idents(idents []ast.Ident) {
	jb.sb.write_string('[\n')
	jb.indent++
	for i, ident in idents {
		jb.write_indent()
		jb.write_ident(ident)
		if i < idents.len - 1 {
			jb.sb.write_string(',')
		}
		jb.sb.write_string('\n')
	}
	jb.indent--
	jb.write_indent()
	jb.sb.write_string(']')
}

fn (mut jb JsonBuilder) write_if_expr(expr ast.IfExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "IfExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"cond": ')
	jb.write_expr(expr.cond)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"stmts": ')
	jb.write_stmts(expr.stmts)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"else_expr": ')
	jb.write_expr(expr.else_expr)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_if_guard_expr(expr ast.IfGuardExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "IfGuardExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"stmt": ')
	jb.write_assign_stmt(expr.stmt)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_index_expr(expr ast.IndexExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "IndexExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"lhs": ')
	jb.write_expr(expr.lhs)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"expr": ')
	jb.write_expr(expr.expr)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"is_gated": ${expr.is_gated}\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_infix_expr(expr ast.InfixExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "InfixExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"op": ')
	jb.write_string(expr.op.str())
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"lhs": ')
	jb.write_expr(expr.lhs)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"rhs": ')
	jb.write_expr(expr.rhs)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"pos": ')
	jb.write_pos(expr.pos)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_init_expr(expr ast.InitExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "InitExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"init_type": ')
	jb.write_expr(expr.typ)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"fields": ')
	jb.write_field_inits(expr.fields)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_keyword(expr ast.Keyword) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "Keyword",\n')

	jb.write_indent()
	jb.sb.write_string('"tok": ')
	jb.write_string(expr.tok.str())
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_keyword_operator(expr ast.KeywordOperator) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "KeywordOperator",\n')

	jb.write_indent()
	jb.sb.write_string('"op": ')
	jb.write_string(expr.op.str())
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"exprs": ')
	jb.write_exprs(expr.exprs)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_lambda_expr(expr ast.LambdaExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "LambdaExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"args": ')
	jb.write_idents(expr.args)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"expr": ')
	jb.write_expr(expr.expr)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_lock_expr(expr ast.LockExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "LockExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"lock_exprs": ')
	jb.write_exprs(expr.lock_exprs)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"rlock_exprs": ')
	jb.write_exprs(expr.rlock_exprs)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"stmts": ')
	jb.write_stmts(expr.stmts)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_map_init_expr(expr ast.MapInitExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "MapInitExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"map_type": ')
	jb.write_expr(expr.typ)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"keys": ')
	jb.write_exprs(expr.keys)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"vals": ')
	jb.write_exprs(expr.vals)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"pos": ')
	jb.write_pos(expr.pos)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_match_expr(expr ast.MatchExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "MatchExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"expr": ')
	jb.write_expr(expr.expr)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"branches": ')
	jb.write_match_branches(expr.branches)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"pos": ')
	jb.write_pos(expr.pos)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_match_branches(branches []ast.MatchBranch) {
	jb.sb.write_string('[\n')
	jb.indent++
	for i, branch in branches {
		jb.write_indent()
		jb.write_match_branch(branch)
		if i < branches.len - 1 {
			jb.sb.write_string(',')
		}
		jb.sb.write_string('\n')
	}
	jb.indent--
	jb.write_indent()
	jb.sb.write_string(']')
}

fn (mut jb JsonBuilder) write_match_branch(branch ast.MatchBranch) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"cond": ')
	jb.write_exprs(branch.cond)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"stmts": ')
	jb.write_stmts(branch.stmts)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"pos": ')
	jb.write_pos(branch.pos)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_modifier_expr(expr ast.ModifierExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "ModifierExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"kind": ')
	jb.write_string(expr.kind.str())
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"expr": ')
	jb.write_expr(expr.expr)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_or_expr(expr ast.OrExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "OrExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"expr": ')
	jb.write_expr(expr.expr)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"stmts": ')
	jb.write_stmts(expr.stmts)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"pos": ')
	jb.write_pos(expr.pos)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_paren_expr(expr ast.ParenExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "ParenExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"expr": ')
	jb.write_expr(expr.expr)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_postfix_expr(expr ast.PostfixExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "PostfixExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"op": ')
	jb.write_string(expr.op.str())
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"expr": ')
	jb.write_expr(expr.expr)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_prefix_expr(expr ast.PrefixExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "PrefixExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"op": ')
	jb.write_string(expr.op.str())
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"expr": ')
	jb.write_expr(expr.expr)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"pos": ')
	jb.write_pos(expr.pos)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_range_expr(expr ast.RangeExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "RangeExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"op": ')
	jb.write_string(expr.op.str())
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"start": ')
	jb.write_expr(expr.start)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"end": ')
	jb.write_expr(expr.end)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_select_expr(expr ast.SelectExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "SelectExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"stmt": ')
	jb.write_stmt(expr.stmt)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"stmts": ')
	jb.write_stmts(expr.stmts)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"next": ')
	jb.write_expr(expr.next)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"pos": ')
	jb.write_pos(expr.pos)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_selector_expr(expr ast.SelectorExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "SelectorExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"lhs": ')
	jb.write_expr(expr.lhs)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"rhs": ')
	jb.write_ident(expr.rhs)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"pos": ')
	jb.write_pos(expr.pos)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_string_inter_literal(expr ast.StringInterLiteral) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "StringInterLiteral",\n')

	jb.write_indent()
	jb.sb.write_string('"kind": ')
	jb.write_string(expr.kind.str())
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"values": [\n')
	jb.indent++
	for i, val in expr.values {
		jb.write_indent()
		jb.write_string(val)
		if i < expr.values.len - 1 {
			jb.sb.write_string(',')
		}
		jb.sb.write_string('\n')
	}
	jb.indent--
	jb.write_indent()
	jb.sb.write_string('],\n')

	jb.write_indent()
	jb.sb.write_string('"inters": ')
	jb.write_string_inters(expr.inters)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_string_inters(inters []ast.StringInter) {
	jb.sb.write_string('[\n')
	jb.indent++
	for i, inter in inters {
		jb.write_indent()
		jb.write_string_inter(inter)
		if i < inters.len - 1 {
			jb.sb.write_string(',')
		}
		jb.sb.write_string('\n')
	}
	jb.indent--
	jb.write_indent()
	jb.sb.write_string(']')
}

fn (mut jb JsonBuilder) write_string_inter(inter ast.StringInter) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"format": ')
	jb.write_string(inter.format.str())
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"width": ${inter.width},\n')

	jb.write_indent()
	jb.sb.write_string('"precision": ${inter.precision},\n')

	jb.write_indent()
	jb.sb.write_string('"expr": ')
	jb.write_expr(inter.expr)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"format_expr": ')
	jb.write_expr(inter.format_expr)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_string_literal(expr ast.StringLiteral) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "StringLiteral",\n')

	jb.write_indent()
	jb.sb.write_string('"kind": ')
	jb.write_string(expr.kind.str())
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"value": ')
	jb.write_string(expr.value)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_tuple(expr ast.Tuple) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "Tuple",\n')

	jb.write_indent()
	jb.sb.write_string('"exprs": ')
	jb.write_exprs(expr.exprs)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_type_expr(expr ast.Type) {
	match expr {
		ast.ArrayType {
			jb.write_array_type(expr)
		}
		ast.ArrayFixedType {
			jb.write_array_fixed_type(expr)
		}
		ast.ChannelType {
			jb.write_channel_type(expr)
		}
		ast.FnType {
			jb.write_fn_type(expr)
		}
		ast.GenericType {
			jb.write_generic_type(expr)
		}
		ast.MapType {
			jb.write_map_type(expr)
		}
		ast.NilType {
			jb.sb.write_string('{"type": "NilType"}')
		}
		ast.NoneType {
			jb.sb.write_string('{"type": "NoneType"}')
		}
		ast.OptionType {
			jb.write_option_type(expr)
		}
		ast.ResultType {
			jb.write_result_type(expr)
		}
		ast.ThreadType {
			jb.write_thread_type(expr)
		}
		ast.TupleType {
			jb.write_tuple_type(expr)
		}
		ast.AnonStructType {
			jb.write_anon_struct_type(expr)
		}
	}
}

fn (mut jb JsonBuilder) write_array_type(typ ast.ArrayType) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "ArrayType",\n')

	jb.write_indent()
	jb.sb.write_string('"elem_type": ')
	jb.write_expr(typ.elem_type)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_array_fixed_type(typ ast.ArrayFixedType) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "ArrayFixedType",\n')

	jb.write_indent()
	jb.sb.write_string('"elem_type": ')
	jb.write_expr(typ.elem_type)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"len": ')
	jb.write_expr(typ.len)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_channel_type(typ ast.ChannelType) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "ChannelType",\n')

	jb.write_indent()
	jb.sb.write_string('"elem_type": ')
	jb.write_expr(typ.elem_type)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"cap": ')
	jb.write_expr(typ.cap)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_fn_type(typ ast.FnType) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "FnType",\n')

	jb.write_indent()
	jb.sb.write_string('"generic_params": ')
	jb.write_exprs(typ.generic_params)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"params": ')
	jb.write_parameters(typ.params)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"return_type": ')
	jb.write_expr(typ.return_type)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_generic_type(typ ast.GenericType) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "GenericType",\n')

	jb.write_indent()
	jb.sb.write_string('"name": ')
	jb.write_expr(typ.name)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"params": ')
	jb.write_exprs(typ.params)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_map_type(typ ast.MapType) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "MapType",\n')

	jb.write_indent()
	jb.sb.write_string('"key_type": ')
	jb.write_expr(typ.key_type)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"value_type": ')
	jb.write_expr(typ.value_type)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_option_type(typ ast.OptionType) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "OptionType",\n')

	jb.write_indent()
	jb.sb.write_string('"base_type": ')
	jb.write_expr(typ.base_type)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_result_type(typ ast.ResultType) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "ResultType",\n')

	jb.write_indent()
	jb.sb.write_string('"base_type": ')
	jb.write_expr(typ.base_type)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_thread_type(typ ast.ThreadType) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "ThreadType",\n')

	jb.write_indent()
	jb.sb.write_string('"elem_type": ')
	jb.write_expr(typ.elem_type)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_tuple_type(typ ast.TupleType) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "TupleType",\n')

	jb.write_indent()
	jb.sb.write_string('"types": ')
	jb.write_exprs(typ.types)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_anon_struct_type(typ ast.AnonStructType) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "AnonStructType",\n')

	jb.write_indent()
	jb.sb.write_string('"generic_params": ')
	jb.write_exprs(typ.generic_params)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"embedded": ')
	jb.write_exprs(typ.embedded)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"fields": ')
	jb.write_field_decls(typ.fields)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_unsafe_expr(expr ast.UnsafeExpr) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"type": "UnsafeExpr",\n')

	jb.write_indent()
	jb.sb.write_string('"stmts": ')
	jb.write_stmts(expr.stmts)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_parameters(params []ast.Parameter) {
	jb.sb.write_string('[\n')
	jb.indent++
	for i, param in params {
		jb.write_indent()
		jb.write_parameter(param)
		if i < params.len - 1 {
			jb.sb.write_string(',')
		}
		jb.sb.write_string('\n')
	}
	jb.indent--
	jb.write_indent()
	jb.sb.write_string(']')
}

fn (mut jb JsonBuilder) write_parameter(param ast.Parameter) {
	jb.sb.write_string('{\n')
	jb.indent++

	jb.write_indent()
	jb.sb.write_string('"name": ')
	jb.write_string(param.name)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"param_type": ')
	jb.write_expr(param.typ)
	jb.sb.write_string(',\n')

	jb.write_indent()
	jb.sb.write_string('"is_mut": ${param.is_mut},\n')

	jb.write_indent()
	jb.sb.write_string('"pos": ')
	jb.write_pos(param.pos)
	jb.sb.write_string('\n')

	jb.indent--
	jb.write_indent()
	jb.sb.write_string('}')
}

fn (mut jb JsonBuilder) write_fn_decls(decls []ast.FnDecl) {
	jb.sb.write_string('[\n')
	jb.indent++
	for i, decl in decls {
		jb.write_indent()
		jb.write_fn_decl(decl)
		if i < decls.len - 1 {
			jb.sb.write_string(',')
		}
		jb.sb.write_string('\n')
	}
	jb.indent--
	jb.write_indent()
	jb.sb.write_string(']')
}

fn (mut jb JsonBuilder) write_pos(pos token.Pos) {
	jb.sb.write_string('${pos.offset}')
}
