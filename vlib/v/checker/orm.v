// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import v.token
import v.util

type ORMExpr = ast.SqlExpr | ast.SqlStmt

enum SqlQueryDataContext {
	where_
	set_
}

fn (mut c Checker) sql_query_data_expr(mut node ast.SqlQueryDataExpr) ast.Type {
	query_data_typ := c.table.find_type('orm.QueryData')
	if query_data_typ == 0 {
		return ast.void_type
	}
	for mut item in node.items {
		c.check_sql_query_data_item(mut item)
	}
	node.typ = query_data_typ
	return query_data_typ
}

fn (mut c Checker) check_sql_query_data_item(mut item ast.SqlQueryDataItem) {
	match mut item {
		ast.SqlQueryDataLeaf {
			c.check_sql_query_data_leaf(item)
		}
		ast.SqlQueryDataIf {
			for branch in item.branches {
				if branch.cond !is ast.EmptyExpr {
					mut cond := branch.cond
					c.expr(mut cond)
				}
				for branch_item in branch.items {
					mut item_copy := branch_item
					c.check_sql_query_data_item(mut item_copy)
				}
			}
		}
	}
}

fn (mut c Checker) check_sql_query_data_leaf(node ast.SqlQueryDataLeaf) {
	mut expr := node.expr
	expr_ := expr.remove_par()
	if expr_ is ast.InfixExpr {
		if !is_sql_query_data_op(expr_.op) {
			c.orm_error('dynamic ORM items must use comparison operators', expr_.pos)
			return
		}
		if !is_sql_query_data_field_candidate(expr_.left) {
			c.orm_error('left side of a dynamic ORM item must be a field name', expr_.left.pos())
		}
		is_nil_comparison := expr_.right is ast.Nil && expr_.op in [.eq, .ne]
		if expr_.op !in [.key_is, .not_is] && !is_nil_comparison {
			mut rhs_expr := expr_.right
			c.expr(mut rhs_expr)
		}
	} else {
		c.orm_error('dynamic ORM items must be comparison expressions', node.pos)
	}
}

fn is_sql_query_data_op(op token.Kind) bool {
	return op in [.eq, .ne, .gt, .lt, .ge, .le, .key_like, .key_ilike, .key_in, .not_in, .key_is,
		.not_is]
}

fn is_sql_query_data_field_candidate(expr ast.Expr) bool {
	return match expr {
		ast.Ident { true }
		ast.SelectorExpr { is_sql_query_data_field_candidate(expr.expr) }
		ast.ParExpr { is_sql_query_data_field_candidate(expr.expr) }
		else { false }
	}
}

fn sql_query_data_field_name(expr ast.Expr) string {
	return match expr {
		ast.Ident { expr.name }
		ast.SelectorExpr { '${sql_query_data_field_name(expr.expr)}.${expr.field_name}' }
		ast.ParExpr { sql_query_data_field_name(expr.expr) }
		else { '' }
	}
}

fn (mut c Checker) resolve_sql_query_data_expr(expr ast.Expr) !ast.SqlQueryDataExpr {
	mut current := expr
	for {
		current = current.remove_par()
		match current {
			ast.SqlQueryDataExpr {
				return current as ast.SqlQueryDataExpr
			}
			ast.Ident {
				obj := current.obj
				match obj {
					ast.Var {
						if obj.is_mut {
							return error('dynamic ORM expressions must use an immutable query-data block alias')
						}
						current = ast.Expr(obj.expr)
						continue
					}
					ast.ConstField {
						current = ast.Expr(obj.expr)
						continue
					}
					else {}
				}
				return error('dynamic ORM expressions must use a query-data block or immutable alias to one')
			}
			else {
				return error('dynamic ORM expressions must use a query-data block or immutable alias to one')
			}
		}
	}
	return error('dynamic ORM expressions must use a query-data block or immutable alias to one')
}

fn (mut c Checker) check_dynamic_sql_query_data(expr ast.Expr, table_sym &ast.TypeSymbol,
	fields []ast.StructField, context SqlQueryDataContext) bool {
	resolved := c.resolve_sql_query_data_expr(expr) or {
		c.orm_error(err.msg(), expr.pos())
		return false
	}
	field_names := fields.map(it.name)
	return c.check_dynamic_sql_query_data_items(resolved.items, table_sym, fields, field_names,
		context)
}

fn (mut c Checker) check_dynamic_sql_query_data_items(items []ast.SqlQueryDataItem, table_sym &ast.TypeSymbol,
	fields []ast.StructField, field_names []string, context SqlQueryDataContext) bool {
	mut ok := true
	for item in items {
		match item {
			ast.SqlQueryDataLeaf {
				mut expr := item.expr
				expr_ := expr.remove_par()
				if expr_ is ast.InfixExpr {
					field_name := sql_query_data_field_name(expr_.left)
					if field_name == '' {
						c.orm_error('left side of a dynamic ORM item must be a field name',
							expr_.left.pos())
						ok = false
						continue
					}
					matched_fields := fields.filter(it.name == field_name)
					if matched_fields.len == 0 {
						c.orm_error(util.new_suggestion(field_name, field_names).say('`${table_sym.name}` structure has no field with name `${field_name}`'),
							expr_.left.pos())
						ok = false
						continue
					}
					field := matched_fields[0]
					match context {
						.where_ {
							if expr_.op in [.and, .logical_or] {
								c.orm_error('dynamic ORM `where` items must use a single comparison expression',
									expr_.pos)
								ok = false
								continue
							}
							if !is_sql_query_data_op(expr_.op) {
								c.orm_error('dynamic ORM `where` items must use comparison operators',
									expr_.pos)
								ok = false
								continue
							}
							mut where_expr := item.expr
							c.expr(mut where_expr)
							c.check_expr_has_no_fn_calls_with_non_orm_return_type(&where_expr)
							c.check_where_expr_has_no_pointless_exprs(table_sym, field_names,
								&where_expr)
						}
						.set_ {
							if expr_.op != .eq {
								c.orm_error('dynamic ORM `set` items must use `==`', expr_.pos)
								ok = false
								continue
							}
							for attr in field.attrs {
								if attr.name == 'fkey' {
									c.orm_error("`${field_name}` is a foreign column of `${table_sym.name}`, it can't update here",
										expr_.pos)
									ok = false
									break
								}
							}
							mut rhs_expr := expr_.right
							old_expected_type := c.expected_type
							c.expected_type = field.typ
							c.expr(mut rhs_expr)
							c.expected_type = old_expected_type
						}
					}
				} else {
					ok = false
					continue
				}
			}
			ast.SqlQueryDataIf {
				for branch in item.branches {
					if branch.cond !is ast.EmptyExpr {
						mut cond := branch.cond
						c.expr(mut cond)
					}
					if !c.check_dynamic_sql_query_data_items(branch.items, table_sym, fields,
						field_names, context) {
						ok = false
					}
				}
			}
		}
	}
	return ok
}

fn (mut c Checker) sql_expr(mut node ast.SqlExpr) ast.Type {
	c.inside_sql = true
	defer {
		c.inside_sql = false
	}

	if !c.check_db_expr(mut node.db_expr) {
		return ast.void_type
	}

	// To avoid panics while working with `table_expr`,
	// it is necessary to check if its type exists.
	if !c.ensure_type_exists(node.table_expr.typ, node.pos) {
		return ast.void_type
	}
	// Keep the SQL expression type aligned with the concretized ORM table type.
	if c.table.cur_fn != unsafe { nil } && c.table.cur_fn.generic_names.len > 0
		&& c.table.cur_concrete_types.len > 0 {
		if c.needs_unwrap_generic_type(node.table_expr.typ) {
			node.table_expr.typ = c.table.unwrap_generic_type(node.table_expr.typ,
				c.table.cur_fn.generic_names, c.table.cur_concrete_types)
		}
		if c.needs_unwrap_generic_type(node.typ) {
			node.typ = c.table.unwrap_generic_type(node.typ, c.table.cur_fn.generic_names,
				c.table.cur_concrete_types)
		}
	}
	table_type := node.table_expr.typ
	table_sym := c.table.sym(table_type)

	if !c.check_orm_table_expr_type(node.table_expr) {
		return ast.void_type
	}

	old_ts := c.cur_orm_ts
	c.cur_orm_ts = *table_sym
	defer {
		c.cur_orm_ts = old_ts
	}

	info := table_sym.info as ast.Struct
	mut fields := c.fetch_and_check_orm_fields(info, node.table_expr.pos, table_sym.name)
	non_primitive_fields := c.get_orm_non_primitive_fields(fields)
	mut sub_structs := map[string]ast.SqlExpr{}

	mut has_primary := false
	mut primary_field := ast.StructField{}

	for field in fields {
		field_typ, field_sym := c.get_non_array_type(field.typ)
		if field_sym.kind == .struct && (field_typ.idx() == node.table_expr.typ.idx()
			|| c.check_recursive_structs(field_sym, table_sym.name)) {
			c.orm_error('invalid recursive struct `${field_sym.name}`', field.pos)
			return ast.void_type
		}

		if field.attrs.contains('primary') {
			if has_primary {
				c.orm_error('a struct can only have one primary key', field.pos)
			}
			has_primary = true
			primary_field = field
		}
	}

	for field in non_primitive_fields {
		field_sym := c.table.sym(field.typ.clear_flag(.option))
		if field_sym.kind == .array && !has_primary {
			c.orm_error('a struct that has a field that holds an array must have a primary key',
				field.pos)
		}

		c.check_orm_non_primitive_struct_field_attrs(field)

		foreign_typ := c.get_field_foreign_table_type(field)

		// Get foreign struct's primary key field
		foreign_sym := c.table.sym(foreign_typ)
		if foreign_sym.info !is ast.Struct {
			continue
		}
		foreign_info := foreign_sym.info as ast.Struct

		// Find primary key field in foreign struct
		mut foreign_primary_field := ast.StructField{}
		mut foreign_has_primary := false
		for f in foreign_info.fields {
			if f.attrs.contains('primary') {
				foreign_primary_field = f
				foreign_has_primary = true
				break
			}
		}

		// Require foreign struct to have a primary key for relationship
		if !foreign_has_primary {
			c.orm_error('struct `${foreign_sym.name}` used as ORM sub-struct field `${field.name}` must have a `@[primary]` field, or use `@[sql: \'-\']` to skip this field',
				field.pos)
			continue
		}

		mut subquery_expr := ast.SqlExpr{
			inserted_var: field.name
			pos:          node.pos
			has_where:    true
			where_expr:   ast.None{}
			typ:          field.typ.clear_flag(.option).set_flag(.result)
			db_expr:      node.db_expr
			table_expr:   ast.TypeNode{
				pos: node.table_expr.pos
				typ: foreign_typ
			}
			is_generated: true
		}

		tmp_inside_sql := c.inside_sql
		c.sql_expr(mut subquery_expr)
		c.inside_sql = tmp_inside_sql

		subquery_expr.where_expr = ast.InfixExpr{
			op:          .eq
			pos:         subquery_expr.pos
			left:        ast.Ident{
				language: .v
				tok_kind: .eq
				scope:    c.fn_scope
				obj:      ast.Var{}
				mod:      'main'
				name:     foreign_primary_field.name
				is_mut:   false
				kind:     .unresolved
				info:     ast.IdentVar{}
			}
			right:       ast.Ident{
				language: .c
				mod:      'main'
				tok_kind: .eq
				obj:      ast.Var{}
				is_mut:   false
				scope:    c.fn_scope
				info:     ast.IdentVar{
					typ: foreign_primary_field.typ
				}
			}
			left_type:   foreign_primary_field.typ
			right_type:  foreign_primary_field.typ
			auto_locked: ''
			or_block:    ast.OrExpr{}
		}

		if field_sym.kind == .array {
			mut where_expr := subquery_expr.where_expr
			if mut where_expr is ast.InfixExpr {
				where_expr.left_type = primary_field.typ
				where_expr.right_type = primary_field.typ

				mut left := where_expr.left
				if mut left is ast.Ident {
					left.name = primary_field.name
				}

				mut right := where_expr.right
				if mut right is ast.Ident {
					mut right_info := right.info
					if mut right_info is ast.IdentVar {
						right_info.typ = primary_field.typ
					}
				}
			}
		}

		sub_structs[field.name] = subquery_expr
	}

	field_names := fields.map(it.name)
	if node.aggregate_kind != .none {
		node.sub_structs = map[string]ast.SqlExpr{}
		if node.aggregate_kind == .count {
			node.fields = [
				ast.StructField{
					typ: ast.int_type
				},
			]
			node.aggregate_field_type = ast.int_type
			node.typ = ast.int_type.set_flag(.result)
		} else {
			aggregate_field := c.check_orm_aggregate_field(node.aggregate_kind,
				node.aggregate_field, fields, table_sym.name, node.pos) or { return ast.void_type }
			node.aggregate_field_type = aggregate_field.typ
			node.fields = [
				aggregate_field,
			]
			node.typ =
				c.orm_aggregate_return_type(node.aggregate_kind, aggregate_field.typ).set_flag(.result)
		}
	} else {
		node.fields = fields
		node.sub_structs = sub_structs.move()
	}

	if node.has_where && !node.is_dynamic {
		c.expr(mut node.where_expr)
		c.check_expr_has_no_fn_calls_with_non_orm_return_type(&node.where_expr)
		c.check_where_expr_has_no_pointless_exprs(table_sym, field_names, &node.where_expr)
	} else if node.has_where && node.is_dynamic {
		c.expr(mut node.where_expr)
		if !c.check_dynamic_sql_query_data(node.where_expr, table_sym, fields, .where_) {
			return ast.void_type
		}
	}

	// Check JOIN clauses
	for mut join in node.joins {
		if !c.check_orm_join_clause(mut join, table_sym) {
			return ast.void_type
		}
	}

	if node.has_order {
		if mut node.order_expr is ast.Ident {
			order_ident_name := node.order_expr.name

			if !table_sym.has_field(order_ident_name) {
				c.orm_error(util.new_suggestion(order_ident_name, field_names).say('`${table_sym.name}` structure has no field with name `${order_ident_name}`'),
					node.order_expr.pos)
				return ast.void_type
			}
		} else {
			c.orm_error("expected `${table_sym.name}` structure's field", node.order_expr.pos())
			return ast.void_type
		}

		c.expr(mut node.order_expr)
	}

	if node.has_limit {
		c.expr(mut node.limit_expr)
		c.check_sql_value_expr_is_comptime_with_natural_number_or_expr_with_int_type(mut node.limit_expr,
			'limit')
	}

	if node.has_offset {
		c.expr(mut node.offset_expr)
		c.check_sql_value_expr_is_comptime_with_natural_number_or_expr_with_int_type(mut node.offset_expr,
			'offset')
	}
	c.expr(mut node.db_expr)
	if node.is_insert {
		node.typ = ast.int_type.set_flag(.result)
	}
	last_cur_or_expr := c.cur_or_expr
	c.cur_or_expr = &node.or_expr
	c.check_orm_or_expr(mut node)
	c.cur_or_expr = last_cur_or_expr

	return node.typ.clear_flag(.result)
}

fn (mut c Checker) sql_stmt(mut node ast.SqlStmt) ast.Type {
	if !c.check_db_expr(mut node.db_expr) {
		return ast.void_type
	}
	node.db_expr_type = c.table.unaliased_type(c.expr(mut node.db_expr))

	for mut line in node.lines {
		c.sql_stmt_line(mut line)
	}
	last_cur_or_expr := c.cur_or_expr
	c.cur_or_expr = &node.or_expr
	c.check_orm_or_expr(mut node)
	c.cur_or_expr = last_cur_or_expr

	return ast.void_type
}

fn (mut c Checker) sql_stmt_line(mut node ast.SqlStmtLine) ast.Type {
	c.inside_sql = true
	defer {
		c.inside_sql = false
	}

	// To avoid panics while working with `table_expr`,
	// it is necessary to check if its type exists.
	if !c.ensure_type_exists(node.table_expr.typ, node.pos) {
		return ast.void_type
	}
	if c.table.cur_fn != unsafe { nil } && c.table.cur_fn.generic_names.len > 0
		&& c.table.cur_concrete_types.len > 0 && c.needs_unwrap_generic_type(node.table_expr.typ) {
		node.table_expr.typ = c.table.unwrap_generic_type(node.table_expr.typ,
			c.table.cur_fn.generic_names, c.table.cur_concrete_types)
	}
	table_type := node.table_expr.typ
	table_sym := c.table.sym(table_type)

	if !c.check_orm_table_expr_type(node.table_expr) {
		return ast.void_type
	}

	old_ts := c.cur_orm_ts
	c.cur_orm_ts = *table_sym
	defer {
		c.cur_orm_ts = old_ts
	}

	inserting_object_name := node.object_var

	if node.kind in [.insert, .upsert] && !node.is_generated {
		inserting_object := node.scope.find(inserting_object_name) or {
			c.error('undefined ident: `${inserting_object_name}`', node.pos)
			return ast.void_type
		}
		mut inserting_object_type := inserting_object.typ

		if inserting_object_type.is_ptr() {
			inserting_object_type = inserting_object.typ.deref()
		}

		if c.table.cur_fn != unsafe { nil } && c.table.cur_fn.generic_names.len > 0
			&& c.table.cur_concrete_types.len > 0
			&& c.needs_unwrap_generic_type(inserting_object_type) {
			inserting_object_type = c.table.unwrap_generic_type(inserting_object_type,
				c.table.cur_fn.generic_names, c.table.cur_concrete_types)
		}

		if inserting_object_type != node.table_expr.typ
			&& !c.table.sumtype_has_variant(inserting_object_type, node.table_expr.typ, false) {
			table_name := table_sym.name
			inserting_type_name := c.table.sym(inserting_object_type).name

			c.error('cannot use `${inserting_type_name}` as `${table_name}`', node.pos)
			return ast.void_type
		}
	}

	if table_sym.info !is ast.Struct {
		c.error('unknown type `${table_sym.name}`', node.pos)
		return ast.void_type
	}

	info := table_sym.info as ast.Struct
	mut fields := c.fetch_and_check_orm_fields(info, node.table_expr.pos, table_sym.name)

	mut insert_fields := []ast.StructField{cap: fields.len}
	for field in fields {
		c.check_orm_struct_field_attrs(node, field)
		// Preserve SQL NULL/default handling for omitted reference fields instead of
		// inserting the V zero value and violating foreign key constraints.
		if field.attrs.contains('references')
			&& c.check_field_of_inserting_struct_is_uninitialized(node, field.name) {
			continue
		}
		insert_fields << field
	}
	fields = insert_fields.clone()

	mut sub_structs := map[int]ast.SqlStmtLine{}
	non_primitive_fields := c.get_orm_non_primitive_fields(fields)

	for field in non_primitive_fields {
		field_typ, field_sym := c.get_non_array_type(field.typ)
		if field_sym.kind == .struct && (field_typ.idx() == node.table_expr.typ.idx()
			|| c.check_recursive_structs(field_sym, table_sym.name)) {
			c.orm_error('invalid recursive struct `${field_sym.name}`', field.pos)
			return ast.void_type
		}

		// Delete an uninitialized struct from fields and skip adding the current field
		// to sub structs to skip inserting an empty struct in the related table.
		if c.check_field_of_inserting_struct_is_uninitialized(node, field.name) {
			fields.delete(fields.index(field))
			continue
		}

		c.check_orm_non_primitive_struct_field_attrs(field)

		foreign_typ := c.get_field_foreign_table_type(field)

		mut subquery_expr := ast.SqlStmtLine{
			pos:          node.pos
			kind:         node.kind
			table_expr:   ast.TypeNode{
				pos: node.table_expr.pos
				typ: foreign_typ
			}
			object_var:   field.name
			is_generated: true
		}

		tmp_inside_sql := c.inside_sql
		c.sql_stmt_line(mut subquery_expr)
		c.inside_sql = tmp_inside_sql
		sub_structs[field.typ] = subquery_expr
	}

	node.fields = fields
	node.sub_structs = sub_structs.move()

	if node.kind == .upsert {
		for field in non_primitive_fields {
			field_typ, field_sym := c.get_non_array_type(field.typ)
			if field_sym.kind == .struct && c.table.sym(field_typ).name == 'time.Time' {
				continue
			}
			c.orm_error('upsert currently supports only primitive, enum, and time.Time fields',
				field.pos)
		}
	}

	for i, column in node.updated_columns {
		updated_fields := node.fields.filter(it.name == column)

		if updated_fields.len == 0 {
			c.orm_error('type `${table_sym.name}` has no field named `${column}`', node.pos)
			continue
		}

		field := updated_fields.first()
		for attr in field.attrs {
			if attr.name == 'fkey' {
				c.orm_error("`${column}` is a foreign column of `${table_sym.name}`, it can't update here",
					node.pos)
				break
			}
		}
		node.updated_columns[i] = c.fetch_field_name(field)
	}

	if node.kind == .update {
		if node.is_dynamic {
			c.expr(mut node.update_data_expr)
			if !c.check_dynamic_sql_query_data(node.update_data_expr, table_sym, node.fields, .set_) {
				return ast.void_type
			}
		} else {
			for i, mut expr in node.update_exprs {
				column := node.updated_columns[i]
				old_expected_type := c.expected_type
				if field := c.get_orm_field_by_column_name(node.fields, column) {
					c.expected_type = field.typ
				}
				c.expr(mut expr)
				c.expected_type = old_expected_type
			}
		}
	}

	if node.where_expr !is ast.EmptyExpr {
		c.expr(mut node.where_expr)
	}

	return ast.void_type
}

fn (mut c Checker) check_orm_struct_field_attrs(node ast.SqlStmtLine, field ast.StructField) {
	for attr in field.attrs {
		if attr.name == 'nonull' {
			c.warn('`nonull` attribute is deprecated; non-optional fields are always "NOT NULL", use Option fields where they can be NULL',
				node.pos)
		}
	}
}

fn (mut c Checker) check_orm_non_primitive_struct_field_attrs(field ast.StructField) {
	field_type := c.table.sym(field.typ.clear_flag(.option))
	mut has_fkey_attr := false

	for attr in field.attrs {
		if attr.name == 'fkey' {
			if field_type.kind != .array && field_type.kind != .struct {
				c.orm_error('the `fkey` attribute must be used only with arrays and structures',
					attr.pos)
				return
			}

			if !attr.has_arg {
				c.orm_error('the `fkey` attribute must have an argument', attr.pos)
				return
			}

			field_struct_type := if field_type.info is ast.Array {
				c.table.sym(field_type.info.elem_type)
			} else {
				field_type
			}

			field_struct_type.find_field(attr.arg) or {
				c.orm_error('`${field_struct_type.name}` struct has no field with name `${attr.arg}`',
					attr.pos)
				return
			}

			has_fkey_attr = true
		}
	}

	if field_type.kind == .array && !has_fkey_attr {
		c.orm_error('a field that holds an array must be defined with the `fkey` attribute',
			field.pos)
	}
}

fn (mut c Checker) fetch_and_check_orm_fields(info ast.Struct, pos token.Pos, table_name string) []ast.StructField {
	if cache := c.orm_table_fields[table_name] {
		return cache
	}
	// Build generic names list from the struct's generic_types
	generic_names := info.generic_types.map(c.table.sym(it).name)
	mut fields := []ast.StructField{}
	for field in info.fields {
		if field.attrs.contains('skip') || field.attrs.contains_arg('sql', '-') {
			continue
		}
		// Skip embedded fields, as their fields are already flattened into the struct
		if field.is_embed {
			embed_sym := c.table.sym(field.typ)
			if embed_sym.info is ast.Struct {
				// fields << c.fetch_and_check_orm_fields(embed_sym.info, pos, embed_sym.name)
				embedded_fields := c.fetch_and_check_orm_fields(embed_sym.info, pos, embed_sym.name)
				for ef in embedded_fields {
					// Ensure the embedded field type is valid (not 0/unresolved)
					if ef.typ == 0 {
						c.orm_error('embedded struct `${embed_sym.name}` has unresolved field type for `${ef.name}`',
							pos)
						continue
					}
					mut new_field := ef
					// Update name for correct C generation (e.g. msg.Payload.field)
					new_field.name = '${field.name}.${ef.name}'
					// Clone attributes to avoid modifying the original struct definition
					new_field.attrs = ef.attrs.clone()

					// Ensure SQL column name matches the original field name
					mut has_sql := false
					for attr in new_field.attrs {
						if attr.name == 'sql' {
							has_sql = true
							break
						}
					}
					if !has_sql {
						new_field.attrs << ast.Attr{
							name:    'sql'
							arg:     ef.name
							has_arg: true
							kind:    .string
						}
					}
					fields << new_field
				}
			}
			continue
		}
		mut field_typ := field.typ
		// Resolve generic field types using the struct's concrete_types if available
		if field_typ.has_flag(.generic) && info.generic_types.len > 0
			&& info.generic_types.len == info.concrete_types.len {
			if resolved_typ := c.table.convert_generic_type(field_typ, generic_names,
				info.concrete_types)
			{
				field_typ = resolved_typ
			}
		}
		// Validate field type is resolved (not 0 and not still generic)
		if field_typ == 0 || field_typ.has_flag(.generic) {
			c.orm_error('field `${field.name}` has unresolved type in generic struct `${table_name}` - use a concrete type instantiation',
				field.pos)
			continue
		}
		if c.orm_field_uses_anon_struct(field_typ) {
			c.orm_error('field `${field.name}` uses an anonymous struct type, which ORM does not support; use a named struct, or skip it with `@[skip]` or `@[sql: \'-\']`',
				field.pos)
			continue
		}
		field_sym := c.table.sym(field_typ)
		final_field_typ := c.table.final_type(field_typ)
		is_primitive := final_field_typ.is_string() || final_field_typ.is_bool()
			|| final_field_typ.is_number()
		is_struct := field_sym.kind == .struct
		is_array := field_sym.kind == .array
		is_enum := field_sym.kind == .enum
		mut is_array_of_structs := false
		if is_array {
			array_info := field_sym.array_info()
			elem_sym := c.table.sym(array_info.elem_type)
			is_array_of_structs = elem_sym.kind == .struct

			if attr := field.attrs.find_first('fkey') {
				if attr.arg == '' {
					c.orm_error('fkey attribute must have an argument', attr.pos)
				}
			} else {
				c.orm_error('array fields must have an fkey attribute', field.pos)
			}
			if array_info.nr_dims > 1 || elem_sym.kind == .array {
				c.orm_error('multi-dimension array fields are not supported', field.pos)
			}
		}
		if attr := field.attrs.find_first('sql') {
			if attr.arg == '' {
				c.orm_error('sql attribute must have an argument', attr.pos)
			}
		}
		if is_primitive || is_struct || is_enum || is_array_of_structs {
			// Use the resolved type in the field
			mut resolved_field := field
			resolved_field.typ = field_typ
			fields << resolved_field
		}
	}
	if fields.len == 0 {
		c.orm_error('select: empty fields in `${table_name}`', pos)
	}
	if attr := info.attrs.find_first('table') {
		if attr.arg == '' {
			c.orm_error('table attribute must have an argument', attr.pos)
		}
	}
	c.orm_table_fields[table_name] = fields
	return fields
}

fn (c &Checker) orm_field_uses_anon_struct(field_typ ast.Type) bool {
	final_field_typ := c.table.final_type(field_typ.clear_flag(.option))
	field_sym := c.table.sym(final_field_typ)
	if field_sym.kind == .struct && field_sym.info is ast.Struct && field_sym.info.is_anon {
		return true
	}
	if field_sym.kind != .array {
		return false
	}
	array_info := field_sym.array_info()
	elem_typ := c.table.final_type(array_info.elem_type.clear_flag(.option))
	elem_sym := c.table.sym(elem_typ)
	return elem_sym.kind == .struct && elem_sym.info is ast.Struct && elem_sym.info.is_anon
}

// check_sql_value_expr_is_comptime_with_natural_number_or_expr_with_int_type checks that an expression is compile-time
// and contains an integer greater than or equal to zero or it is a runtime expression with an integer type.
fn (mut c Checker) check_sql_value_expr_is_comptime_with_natural_number_or_expr_with_int_type(mut expr ast.Expr,
	sql_keyword string) {
	comptime_number := c.get_comptime_number_value(mut expr) or {
		c.check_sql_expr_type_is_int(expr, sql_keyword)
		return
	}

	if comptime_number < 0 {
		c.orm_error('`${sql_keyword}` must be greater than or equal to zero', expr.pos())
	}
}

fn (mut c Checker) check_orm_aggregate_field(kind ast.SqlAggregateKind, field_name string,
	fields []ast.StructField, table_name string, pos token.Pos) ?ast.StructField {
	field := fields.filter(it.name == field_name)
	if field.len == 0 {
		mut field_names := []string{cap: fields.len}
		for item in fields {
			field_names << item.name
		}
		c.orm_error(util.new_suggestion(field_name, field_names).say('`${table_name}` structure has no field with name `${field_name}`'),
			pos)
		return none
	}
	resolved_field := field[0]
	field_type := c.table.final_type(resolved_field.typ.clear_flag(.option))
	field_sym := c.table.sym(field_type)
	is_time := field_sym.name == 'time.Time'
	is_numeric := field_type.is_number()
	is_string := field_type.is_string()

	if field_sym.kind in [.array, .struct] && !is_time {
		c.orm_error('ORM aggregate functions do not support array or sub-struct fields', pos)
		return none
	}

	match kind {
		.sum, .avg {
			if !is_numeric {
				msg := match kind {
					.sum { '`sum` aggregate requires a numeric field' }
					.avg { '`avg` aggregate requires a numeric field' }
					else { 'aggregate requires a numeric field' }
				}
				c.orm_error(msg, pos)
				return none
			}
		}
		.min, .max {
			if !(is_numeric || is_string || is_time) {
				msg := match kind {
					.min { '`min` aggregate requires a numeric, string, or time.Time field' }
					.max { '`max` aggregate requires a numeric, string, or time.Time field' }
					else { 'aggregate requires a numeric, string, or time.Time field' }
				}
				c.orm_error(msg, pos)
				return none
			}
		}
		else {}
	}
	return resolved_field
}

fn (_ &Checker) orm_aggregate_return_type(kind ast.SqlAggregateKind, field_type ast.Type) ast.Type {
	return match kind {
		.count { ast.int_type }
		.avg { ast.f64_type.set_flag(.option) }
		.sum, .min, .max { field_type.clear_flag(.option).set_flag(.option) }
		.none { ast.void_type }
	}
}

fn (mut c Checker) check_sql_expr_type_is_int(expr &ast.Expr, sql_keyword string) {
	if expr is ast.Ident {
		if expr.obj.typ.is_int() {
			return
		}
	} else if expr is ast.SelectorExpr {
		if expr.typ.is_int() {
			return
		}
	} else if expr is ast.CallExpr {
		if expr.return_type == 0 {
			return
		}

		type_symbol := c.table.sym(expr.return_type)
		is_error_type := expr.return_type.has_flag(.result) || expr.return_type.has_flag(.option)
		is_acceptable_type := type_symbol.is_int() && !is_error_type

		if !is_acceptable_type {
			error_type_symbol := c.fn_return_type_flag_to_string(expr.return_type)
			c.orm_error('function calls in `${sql_keyword}` must return only an integer type, but `${expr.name}` returns `${error_type_symbol}${type_symbol.name}`',
				expr.pos)
		}

		return
	} else if expr is ast.ParExpr {
		c.check_sql_expr_type_is_int(expr.expr, sql_keyword)
		return
	}

	c.orm_error('the type of `${sql_keyword}` must be an integer type', expr.pos())
}

fn (mut c Checker) orm_error(message string, pos token.Pos) {
	c.error('ORM: ${message}', pos)
}

// check_expr_has_no_fn_calls_with_non_orm_return_type checks that an expression has no function calls
// that return complex types which can't be transformed into SQL.
fn (mut c Checker) check_expr_has_no_fn_calls_with_non_orm_return_type(expr &ast.Expr) {
	if expr is ast.CallExpr {
		// `expr.return_type` may be empty. For example, a user call function incorrectly without passing all required arguments.
		// This error will be handled in another place. Otherwise, `c.table.sym` below does panic.
		//
		// fn test(flag bool) {}
		// test()
		//      ~~~~~~ expected 1 arguments, but got 0
		if expr.return_type == 0 {
			return
		}

		type_symbol := c.table.sym(expr.return_type)
		is_time := type_symbol.cname == 'time__Time'
		is_not_pointer := !type_symbol.is_pointer()
		is_error_type := expr.return_type.has_flag(.result) || expr.return_type.has_flag(.option)
		is_acceptable_type := (type_symbol.is_primitive() || is_time) && is_not_pointer
			&& !is_error_type

		if !is_acceptable_type {
			error_type_symbol := c.fn_return_type_flag_to_string(expr.return_type)
			c.orm_error('function calls must return only primitive types and time.Time, but `${expr.name}` returns `${error_type_symbol}${type_symbol.name}`',
				expr.pos)
		}
	} else if expr is ast.ParExpr {
		c.check_expr_has_no_fn_calls_with_non_orm_return_type(expr.expr)
	} else if expr is ast.InfixExpr {
		c.check_expr_has_no_fn_calls_with_non_orm_return_type(expr.left)
		c.check_expr_has_no_fn_calls_with_non_orm_return_type(expr.right)
		if expr.right_type.has_flag(.option) && expr.op !in [.key_is, .not_is] {
			c.warn('comparison with Option value probably isn\'t intended; use "is none" and "!is none" to select by NULL',
				expr.pos)
		} else if expr.right_type == ast.none_type && expr.op !in [.key_is, .not_is] {
			c.warn('comparison with none probably isn\'t intended; use "is none" and "!is none" to select by NULL',
				expr.pos)
		}
	}
}

// check_where_data_expr_has_no_struct_field_refs checks that expressions destined for ORM bind data
// do not reference fields from the queried table, because ORM where clauses currently only support
// comparing a table field with a V value, not with another table field.
fn (mut c Checker) check_where_data_expr_has_no_struct_field_refs(table_type_symbol &ast.TypeSymbol, expr ast.Expr, op token.Kind) {
	match expr {
		ast.Ident {
			if expr.kind == .unresolved && table_type_symbol.has_field(expr.name) {
				c.orm_error('right side of the `${op}` expression cannot reference another `${table_type_symbol.name}` field; field-to-field comparisons are not supported',
					expr.pos)
			}
		}
		ast.ArrayInit {
			for item in expr.exprs {
				c.check_where_data_expr_has_no_struct_field_refs(table_type_symbol, item, op)
			}
		}
		ast.CallExpr {
			c.check_where_data_expr_has_no_struct_field_refs(table_type_symbol, expr.left, op)
			for arg in expr.args {
				c.check_where_data_expr_has_no_struct_field_refs(table_type_symbol, arg.expr, op)
			}
		}
		ast.CastExpr {
			c.check_where_data_expr_has_no_struct_field_refs(table_type_symbol, expr.expr, op)
			c.check_where_data_expr_has_no_struct_field_refs(table_type_symbol, expr.arg, op)
		}
		ast.IndexExpr {
			c.check_where_data_expr_has_no_struct_field_refs(table_type_symbol, expr.left, op)
			c.check_where_data_expr_has_no_struct_field_refs(table_type_symbol, expr.index, op)
		}
		ast.InfixExpr {
			c.check_where_data_expr_has_no_struct_field_refs(table_type_symbol, expr.left, op)
			c.check_where_data_expr_has_no_struct_field_refs(table_type_symbol, expr.right, op)
		}
		ast.MapInit {
			for key in expr.keys {
				c.check_where_data_expr_has_no_struct_field_refs(table_type_symbol, key, op)
			}
			for val in expr.vals {
				c.check_where_data_expr_has_no_struct_field_refs(table_type_symbol, val, op)
			}
		}
		ast.ParExpr {
			c.check_where_data_expr_has_no_struct_field_refs(table_type_symbol, expr.expr, op)
		}
		ast.PrefixExpr {
			c.check_where_data_expr_has_no_struct_field_refs(table_type_symbol, expr.right, op)
		}
		ast.SelectorExpr {
			table_name := util.strip_mod_name(table_type_symbol.name)
			if table_type_symbol.has_field(expr.field_name) {
				if expr.expr is ast.TypeNode
					&& c.table.sym(expr.expr.typ).name == table_type_symbol.name {
					c.orm_error('right side of the `${op}` expression cannot reference another `${table_type_symbol.name}` field; field-to-field comparisons are not supported',
						expr.pos)
				} else if expr.expr is ast.Ident && expr.expr.kind == .unresolved
					&& expr.expr.name == table_name {
					c.orm_error('right side of the `${op}` expression cannot reference another `${table_type_symbol.name}` field; field-to-field comparisons are not supported',
						expr.pos)
				}
			}
			c.check_where_data_expr_has_no_struct_field_refs(table_type_symbol, expr.expr, op)
		}
		ast.StringInterLiteral {
			for interpolated in expr.exprs {
				c.check_where_data_expr_has_no_struct_field_refs(table_type_symbol, interpolated,
					op)
			}
		}
		ast.StructInit {
			for init_field in expr.init_fields {
				c.check_where_data_expr_has_no_struct_field_refs(table_type_symbol,
					init_field.expr, op)
			}
		}
		ast.UnsafeExpr {
			c.check_where_data_expr_has_no_struct_field_refs(table_type_symbol, expr.expr, op)
		}
		else {}
	}
}

// check_where_expr_has_no_pointless_exprs checks that an expression has no pointless expressions
// which don't affect the result. For example, `where 3` is pointless.
// Also, it checks that the left side of the infix expression is always the structure field.
fn (mut c Checker) check_where_expr_has_no_pointless_exprs(table_type_symbol &ast.TypeSymbol, field_names []string,
	expr &ast.Expr) {
	// Skip type checking for generated subqueries
	// that are not linked to scope and vars but only created for cgen.
	if expr is ast.None {
		return
	}

	if expr is ast.InfixExpr {
		has_no_field_error := "left side of the `${expr.op}` expression must be one of the `${table_type_symbol.name}`'s fields"

		if expr.left is ast.Ident {
			left_ident_name := expr.left.name

			if !table_type_symbol.has_field(left_ident_name) {
				c.orm_error(util.new_suggestion(left_ident_name, field_names).say(has_no_field_error),
					expr.left.pos)
			}
		} else if expr.left is ast.InfixExpr || expr.left is ast.ParExpr
			|| expr.left is ast.PrefixExpr {
			c.check_where_expr_has_no_pointless_exprs(table_type_symbol, field_names, expr.left)
		} else if !(expr.left is ast.SelectorExpr
			&& c.comptime.is_comptime_selector_field_name(expr.left, 'name')) {
			c.orm_error(has_no_field_error, expr.left.pos())
		}

		if expr.op in [.ne, .eq, .lt, .gt, .ge, .le, .key_like, .key_ilike, .key_in, .not_in] {
			c.check_where_data_expr_has_no_struct_field_refs(table_type_symbol, expr.right, expr.op)
		} else if expr.right is ast.InfixExpr || expr.right is ast.ParExpr
			|| expr.right is ast.PrefixExpr {
			c.check_where_expr_has_no_pointless_exprs(table_type_symbol, field_names, expr.right)
		}
	} else if expr is ast.ParExpr {
		c.check_where_expr_has_no_pointless_exprs(table_type_symbol, field_names, expr.expr)
	} else if expr is ast.PrefixExpr {
		c.check_where_expr_has_no_pointless_exprs(table_type_symbol, field_names, expr.right)
	} else {
		c.orm_error('`where` expression must have at least one comparison for filtering rows',
			expr.pos())
	}
}

fn (_ &Checker) fn_return_type_flag_to_string(typ ast.Type) string {
	is_result_type := typ.has_flag(.result)
	is_option_type := typ.has_flag(.option)

	return if is_result_type {
		'!'
	} else if is_option_type {
		'?'
	} else {
		''
	}
}

fn (mut c Checker) check_orm_or_expr(mut expr ORMExpr) {
	if mut expr is ast.SqlExpr {
		if expr.is_generated {
			return
		}
	}
	return_type := if mut expr is ast.SqlExpr {
		expr.typ
	} else {
		ast.void_type.set_flag(.result)
	}

	if expr.or_expr.kind == .absent {
		if c.inside_defer {
			c.error('ORM returns a result, so it should have an `or {}` block at the end', expr.pos)
		} else {
			c.error('ORM returns a result, so it should have either an `or {}` block, or `!` at the end',
				expr.pos)
		}
	} else {
		c.check_or_expr(expr.or_expr, return_type.clear_flag(.result), return_type, if mut expr is ast.SqlExpr {
			expr
		} else {
			ast.empty_expr
		})
	}

	if expr.or_expr.kind == .block {
		c.expected_or_type = return_type.clear_flag(.result)
		c.stmts_ending_with_expression(mut expr.or_expr.stmts, c.expected_or_type)
		c.expected_or_type = ast.void_type
	}
}

// check_db_expr checks the `db_expr` implements `orm.Connection` and has no `option` flag.
fn (mut c Checker) check_db_expr(mut db_expr ast.Expr) bool {
	connection_type_index := c.table.find_type('orm.Connection')
	connection_typ := connection_type_index
	db_expr_type := c.expr(mut db_expr)

	// If we didn't find `orm.Connection`, we don't have any imported modules
	// that depend on `orm` and implement the `orm.Connection` interface.
	if connection_type_index == 0 {
		c.error('expected a type that implements the `orm.Connection` interface', db_expr.pos())
		return false
	}

	is_implemented := c.type_implements(db_expr_type, connection_typ, db_expr.pos())
	is_option := db_expr_type.has_flag(.option)

	if is_implemented && is_option {
		c.error(c.expected_msg(db_expr_type, db_expr_type.clear_flag(.option)), db_expr.pos())
		return false
	}

	return true
}

fn (mut c Checker) check_orm_table_expr_type(type_node &ast.TypeNode) bool {
	table_sym := c.table.sym(type_node.typ)

	if table_sym.info !is ast.Struct {
		c.orm_error('the table symbol `${table_sym.name}` has to be a struct', type_node.pos)

		return false
	}

	return true
}

// get_field_foreign_table_type gets the type of table in which the primary key
// is referred to by the provided field.  For example, the `[]Child` field
// refers to the foreign table `Child`.
fn (c &Checker) get_field_foreign_table_type(table_field &ast.StructField) ast.Type {
	field_type := table_field.typ.clear_flag(.option)
	field_sym := c.table.sym(field_type)
	if field_sym.kind == .struct {
		return field_type
	} else if field_sym.kind == .array {
		return field_sym.array_info().elem_type
	} else {
		return ast.no_type
	}
}

// get_orm_non_primitive_fields filters the table fields by selecting only
// non-primitive fields such as arrays and structs.
fn (c &Checker) get_orm_non_primitive_fields(fields []ast.StructField) []ast.StructField {
	mut res := []ast.StructField{}
	for field in fields {
		type_with_no_option_flag := field.typ.clear_flag(.option)
		field_sym := c.table.sym(type_with_no_option_flag)
		is_struct := field_sym.kind == .struct
		is_array := field_sym.kind == .array
		is_array_with_struct_elements := is_array
			&& c.table.sym(field_sym.array_info().elem_type).kind == .struct
		is_time := c.table.get_type_name(type_with_no_option_flag) == 'time.Time'

		if (is_struct || is_array_with_struct_elements) && !is_time {
			res << field
		}
	}
	return res
}

fn (mut c Checker) get_orm_field_by_column_name(fields []ast.StructField, column string) ?ast.StructField {
	for field in fields {
		if c.fetch_field_name(field) == column {
			return field
		}
	}
	return none
}

// walkingdevel: Now I don't think it's a good solution
// because it only checks structure initialization,
// but structure fields may be updated later before inserting.
// For example,
// ```v
// mut package := Package{
// 	name: 'xml'
// }
//
// package.author = User{
// 	username: 'walkingdevel'
// }
// ```
// TODO: rewrite it, move to runtime.
fn (_ &Checker) check_field_of_inserting_struct_is_uninitialized(node &ast.SqlStmtLine, field_name string) bool {
	struct_scope := node.scope.find_var(node.object_var) or { return false }

	if struct_scope.expr is ast.StructInit {
		return struct_scope.expr.init_fields.filter(it.name == field_name).len == 0
	}

	return false
}

// check_recursive_structs returns true if type is struct and has any child or nested child with the type of the given struct name,
// array elements are all checked.
fn (mut c Checker) check_recursive_structs(ts &ast.TypeSymbol, struct_name string) bool {
	if ts.info is ast.Struct {
		for field in ts.info.fields {
			_, field_sym := c.get_non_array_type(field.typ)
			if field_sym.kind == .struct && field_sym.name == struct_name {
				return true
			}
		}
	}
	return false
}

// returns the final non-array type by recursively retrieving the element type of an array type,
// if the input type is not an array, it is returned directly.
fn (mut c Checker) get_non_array_type(typ_ ast.Type) (ast.Type, &ast.TypeSymbol) {
	mut typ := typ_
	mut sym := c.table.sym(typ)
	for {
		if sym.kind == .array {
			typ = sym.array_info().elem_type
			sym = c.table.sym(typ)
		} else {
			break
		}
	}
	return typ, sym
}

// check_orm_join_clause validates a JOIN clause in an ORM query.
// It checks that the joined table type exists and is a struct,
// and validates the ON expression.
fn (mut c Checker) check_orm_join_clause(mut join ast.JoinClause, main_table_sym &ast.TypeSymbol) bool {
	// Check that the joined table type exists
	if !c.ensure_type_exists(join.table_expr.typ, join.pos) {
		return false
	}

	join_table_sym := c.table.sym(join.table_expr.typ)

	// Check that the joined table is a struct
	if join_table_sym.info !is ast.Struct {
		c.orm_error('JOIN table `${join_table_sym.name}` must be a struct type', join.pos)
		return false
	}

	// Validate the ON expression structure without running full expression checking
	// (since Table.field syntax is special in ORM context)
	return c.check_orm_join_on_expr(join.on_expr, main_table_sym, join_table_sym)
}

// check_orm_join_on_expr validates the ON expression of a JOIN clause.
// It expects the form: TableA.fieldA == TableB.fieldB
// where TableA is either the main table or the joined table.
fn (mut c Checker) check_orm_join_on_expr(on_expr ast.Expr, main_table_sym &ast.TypeSymbol, join_table_sym &ast.TypeSymbol) bool {
	// The ON expression should be an infix expression (e.g., Table1.field == Table2.field)
	if on_expr is ast.InfixExpr {
		// Check that the operator is a comparison operator
		if on_expr.op !in [.eq, .ne, .lt, .gt, .le, .ge] {
			c.orm_error('JOIN ON condition must use a comparison operator (==, !=, <, >, <=, >=)',
				on_expr.pos)
			return false
		}

		// Check left side (should be Table.field format)
		if !c.check_orm_join_field_ref(on_expr.left, main_table_sym, join_table_sym) {
			return false
		}

		// Check right side (should be Table.field format)
		if !c.check_orm_join_field_ref(on_expr.right, main_table_sym, join_table_sym) {
			return false
		}

		return true
	} else if on_expr !is ast.EmptyExpr {
		c.orm_error('JOIN ON condition must be a comparison expression (e.g., Table1.field == Table2.field)',
			on_expr.pos())
		return false
	}

	return true
}

// check_orm_join_field_ref validates that an expression is a valid Table.field reference
// for a JOIN ON condition. The table must be either the main table or the joined table.
fn (mut c Checker) check_orm_join_field_ref(expr ast.Expr, main_table_sym &ast.TypeSymbol, join_table_sym &ast.TypeSymbol) bool {
	// Handle SelectorExpr (e.g., User.department_id)
	if expr is ast.SelectorExpr {
		// Get the table name from the selector's left side
		mut table_name := ''
		if expr.expr is ast.Ident {
			table_name = expr.expr.name
		} else if expr.expr is ast.TypeNode {
			table_name = c.table.sym(expr.expr.typ).name
		}

		// Check if the table name matches either the main table or the joined table
		main_table_name := util.strip_mod_name(main_table_sym.name)
		join_table_name := util.strip_mod_name(join_table_sym.name)

		// Determine which table to check the field against
		is_main_table := table_name == main_table_name
		is_join_table := table_name == join_table_name

		if !is_main_table && !is_join_table {
			c.orm_error('table `${table_name}` in JOIN ON condition must be either `${main_table_name}` or `${join_table_name}`',
				expr.pos)
			return false
		}

		// Check if the field exists in the target table
		field_name := expr.field_name
		if is_main_table {
			if !main_table_sym.has_field(field_name) {
				c.orm_error('field `${field_name}` does not exist in table `${main_table_name}`',
					expr.pos)
				return false
			}
		} else {
			if !join_table_sym.has_field(field_name) {
				c.orm_error('field `${field_name}` does not exist in table `${join_table_name}`',
					expr.pos)
				return false
			}
		}

		return true
	}

	// Handle EnumVal - this happens when the parser sees Type.field as an enum access
	// In ORM context, we need to reinterpret this as a table field reference
	if expr is ast.EnumVal {
		// The enum_name is the table name (e.g., "User")
		// The val is the field name (e.g., "department_id")
		table_name := util.strip_mod_name(expr.enum_name)
		field_name := expr.val

		main_table_name := util.strip_mod_name(main_table_sym.name)
		join_table_name := util.strip_mod_name(join_table_sym.name)

		is_main_table := table_name == main_table_name
		is_join_table := table_name == join_table_name

		if !is_main_table && !is_join_table {
			c.orm_error('table `${table_name}` in JOIN ON condition must be either `${main_table_name}` or `${join_table_name}`',
				expr.pos)
			return false
		}

		// Check if the field exists in the target table
		if is_main_table {
			if !main_table_sym.has_field(field_name) {
				c.orm_error('field `${field_name}` does not exist in table `${main_table_name}`',
					expr.pos)
				return false
			}
		} else {
			if !join_table_sym.has_field(field_name) {
				c.orm_error('field `${field_name}` does not exist in table `${join_table_name}`',
					expr.pos)
				return false
			}
		}

		return true
	}

	c.orm_error('JOIN ON condition expects Table.field format (got ${typeof(expr).name})',
		expr.pos())
	return false
}
