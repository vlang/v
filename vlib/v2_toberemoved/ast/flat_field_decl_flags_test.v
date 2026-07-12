// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import v2.token

fn field_flags_ident_expr(name string) Expr {
	return Expr(Ident{
		name: name
	})
}

fn field_flags_int_expr() Expr {
	return field_flags_ident_expr('int')
}

fn make_field_flags_file() File {
	return File{
		name:  'field_flags.v'
		mod:   'field_flags'
		stmts: [
			Stmt(ModuleStmt{
				name: 'field_flags'
			}),
			Stmt(StructDecl{
				name:   'Config'
				fields: [
					FieldDecl{
						name:      'public_value'
						typ:       field_flags_int_expr()
						is_public: true
					},
					FieldDecl{
						name:   'mutable_value'
						typ:    field_flags_int_expr()
						is_mut: true
					},
					FieldDecl{
						name:          'module_mut_value'
						typ:           field_flags_int_expr()
						is_module_mut: true
					},
				]
			}),
			Stmt(InterfaceDecl{
				name:   'Runner'
				fields: [
					FieldDecl{
						name:                'run'
						typ:                 field_flags_int_expr()
						is_interface_method: true
					},
				]
			}),
		]
	}
}

fn test_field_decl_flags_survive_flat_roundtrip() {
	mut b := new_flat_builder()
	b.append_file(make_field_flags_file())
	files := b.flat.to_files()
	assert files.len == 1
	assert files[0].stmts.len == 3
	assert files[0].stmts[1] is StructDecl
	struct_decl := files[0].stmts[1] as StructDecl
	assert struct_decl.fields.len == 3
	assert struct_decl.fields[0].is_public
	assert !struct_decl.fields[0].is_mut
	assert !struct_decl.fields[0].is_module_mut
	assert !struct_decl.fields[0].is_interface_method
	assert !struct_decl.fields[1].is_public
	assert struct_decl.fields[1].is_mut
	assert !struct_decl.fields[1].is_module_mut
	assert !struct_decl.fields[1].is_interface_method
	assert !struct_decl.fields[2].is_public
	assert !struct_decl.fields[2].is_mut
	assert struct_decl.fields[2].is_module_mut
	assert !struct_decl.fields[2].is_interface_method
	assert files[0].stmts[2] is InterfaceDecl
	interface_decl := files[0].stmts[2] as InterfaceDecl
	assert interface_decl.fields.len == 1
	assert !interface_decl.fields[0].is_public
	assert !interface_decl.fields[0].is_mut
	assert !interface_decl.fields[0].is_module_mut
	assert interface_decl.fields[0].is_interface_method
}

fn test_global_decl_public_flag_survives_flat_roundtrip() {
	mut b := new_flat_builder()
	b.append_file(File{
		name:  'global_decl_public.v'
		mod:   'field_flags'
		stmts: [
			Stmt(ModuleStmt{
				name: 'field_flags'
			}),
			Stmt(GlobalDecl{
				is_public: true
				fields:    [
					FieldDecl{
						name: 'counter'
						typ:  field_flags_int_expr()
					},
				]
			}),
		]
	})
	files := b.flat.to_files()
	assert files[0].stmts.len == 2
	assert files[0].stmts[1] is GlobalDecl
	global_decl := files[0].stmts[1] as GlobalDecl
	assert global_decl.is_public
	assert global_decl.fields.len == 1
}

fn test_emit_field_decl_by_ids_preserves_field_flags() {
	field := FieldDecl{
		name:      'global_counter'
		typ:       field_flags_int_expr()
		is_public: true
		is_mut:    true
	}
	mut b := new_flat_builder()
	b.append_file(File{
		name:  'direct_field_flags.v'
		mod:   'field_flags'
		stmts: [
			Stmt(ModuleStmt{
				name: 'field_flags'
			}),
		]
	})
	typ_id := b.emit_expr(field.typ)
	value_id := b.emit_expr(field.value)
	attrs_id := b.emit_attribute_list(field.attributes)
	field_id := b.emit_field_decl_by_ids(field, typ_id, value_id, attrs_id)
	fields_id := b.emit_aux_list_from_ids([field_id])
	decl_attrs_id := b.emit_attribute_list([])
	global_id := b.emit_global_decl_by_ids(true, decl_attrs_id, fields_id)
	b.append_file_stmts(0, [global_id])
	files := b.flat.to_files()
	assert files[0].stmts.len == 2
	assert files[0].stmts[1] is GlobalDecl
	global_decl := files[0].stmts[1] as GlobalDecl
	assert global_decl.is_public
	assert global_decl.fields.len == 1
	assert global_decl.fields[0].is_public
	assert global_decl.fields[0].is_mut
	assert !global_decl.fields[0].is_module_mut
	assert !global_decl.fields[0].is_interface_method
}

fn field_flags_array_update_from_stmt(stmt Stmt) ArrayInitExpr {
	assert stmt is ExprStmt
	expr_stmt := stmt as ExprStmt
	assert expr_stmt.expr is ArrayInitExpr
	return expr_stmt.expr as ArrayInitExpr
}

fn test_array_init_update_expr_survives_flat_roundtrip() {
	mut b := new_flat_builder()
	b.append_file(File{
		name:  'array_update_expr.v'
		mod:   'field_flags'
		stmts: [
			Stmt(ModuleStmt{
				name: 'field_flags'
			}),
			Stmt(ExprStmt{
				expr: Expr(ArrayInitExpr{
					typ:         field_flags_int_expr()
					update_expr: field_flags_ident_expr('base')
					exprs:       [
						field_flags_ident_expr('item'),
					]
				})
			}),
		]
	})
	files := b.flat.to_files()
	assert files[0].stmts.len == 2
	array_expr := field_flags_array_update_from_stmt(files[0].stmts[1])
	assert array_expr.update_expr is Ident
	assert (array_expr.update_expr as Ident).name == 'base'
	assert array_expr.exprs.len == 1
	assert array_expr.exprs[0] is Ident
	assert (array_expr.exprs[0] as Ident).name == 'item'
}

fn test_emit_array_init_expr_by_ids_preserves_update_expr() {
	mut b := new_flat_builder()
	b.append_file(File{
		name:  'direct_array_update_expr.v'
		mod:   'field_flags'
		stmts: [
			Stmt(ModuleStmt{
				name: 'field_flags'
			}),
		]
	})
	typ_id := b.emit_expr(field_flags_int_expr())
	init_id := b.emit_expr(empty_expr)
	cap_id := b.emit_expr(empty_expr)
	len_id := b.emit_expr(empty_expr)
	update_expr_id := b.emit_expr(field_flags_ident_expr('base'))
	elem_id := b.emit_expr(field_flags_ident_expr('item'))
	array_id := b.emit_array_init_expr_by_ids(typ_id, init_id, cap_id, len_id, update_expr_id, [
		elem_id,
	], token.Pos{})
	stmt_id := b.emit_expr_stmt_by_id(array_id)
	b.append_file_stmts(0, [stmt_id])
	files := b.flat.to_files()
	assert files[0].stmts.len == 2
	array_expr := field_flags_array_update_from_stmt(files[0].stmts[1])
	assert array_expr.update_expr is Ident
	assert (array_expr.update_expr as Ident).name == 'base'
	assert array_expr.exprs.len == 1
	assert array_expr.exprs[0] is Ident
	assert (array_expr.exprs[0] as Ident).name == 'item'
}
