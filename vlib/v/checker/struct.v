// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast

pub fn (mut c Checker) struct_decl(mut node ast.StructDecl) {
	mut struct_sym, struct_typ_idx := c.table.find_sym_and_type_idx(node.name)
	mut has_generic_types := false
	if mut struct_sym.info is ast.Struct {
		if node.language == .v && !c.is_builtin_mod && !struct_sym.info.is_anon {
			c.check_valid_pascal_case(node.name, 'struct name', node.pos)
		}
		for embed in node.embeds {
			if embed.typ.has_flag(.generic) {
				has_generic_types = true
			}
			embed_sym := c.table.sym(embed.typ)
			if embed_sym.kind != .struct_ {
				c.error('`$embed_sym.name` is not a struct', embed.pos)
			} else {
				info := embed_sym.info as ast.Struct
				if info.is_heap && !embed.typ.is_ptr() {
					struct_sym.info.is_heap = true
				}
			}
		}
		if struct_sym.info.is_minify {
			node.fields.sort_with_compare(minify_sort_fn)
			struct_sym.info.fields.sort_with_compare(minify_sort_fn)
		}
		for attr in node.attrs {
			if attr.name == 'typedef' && node.language != .c {
				c.error('`typedef` attribute can only be used with C structs', node.pos)
			}
		}
		for i, field in node.fields {
			c.ensure_type_exists(field.typ, field.type_pos) or { return }
			if field.typ.has_flag(.generic) {
				has_generic_types = true
			}
			if node.language == .v {
				c.check_valid_snake_case(field.name, 'field name', field.pos)
			}
			sym := c.table.sym(field.typ)
			for j in 0 .. i {
				if field.name == node.fields[j].name {
					c.error('field name `$field.name` duplicate', field.pos)
				}
			}
			if field.typ != 0 {
				if !field.typ.is_ptr() {
					if c.table.unaliased_type(field.typ) == struct_typ_idx {
						c.error('field `$field.name` is part of `$node.name`, they can not both have the same type',
							field.type_pos)
					}
				}
				field_sym := c.table.sym(field.typ)
				if field_sym.kind == .function {
					fn_info := field_sym.info as ast.FnType
					c.ensure_type_exists(fn_info.func.return_type, fn_info.func.return_type_pos) or {
						return
					}
					for param in fn_info.func.params {
						c.ensure_type_exists(param.typ, param.type_pos) or { return }
					}
				}
			}
			if sym.kind == .struct_ {
				info := sym.info as ast.Struct
				if info.is_heap && !field.typ.is_ptr() {
					struct_sym.info.is_heap = true
				}
			}
			if field.has_default_expr {
				c.expected_type = field.typ
				mut field_expr_type := c.expr(field.default_expr)
				if !field.typ.has_flag(.optional) {
					c.check_expr_opt_call(field.default_expr, field_expr_type)
				}
				struct_sym.info.fields[i].default_expr_typ = field_expr_type
				c.check_expected(field_expr_type, field.typ) or {
					if sym.kind == .interface_
						&& c.type_implements(field_expr_type, field.typ, field.pos) {
						if !field_expr_type.is_ptr() && !field_expr_type.is_pointer()
							&& !c.inside_unsafe {
							field_expr_type_sym := c.table.sym(field_expr_type)
							if field_expr_type_sym.kind != .interface_ {
								c.mark_as_referenced(mut &node.fields[i].default_expr,
									true)
							}
						}
					} else {
						c.error('incompatible initializer for field `$field.name`: $err.msg()',
							field.default_expr.pos())
					}
				}
				// Check for unnecessary inits like ` = 0` and ` = ''`
				if field.typ.is_ptr() {
					if field.default_expr is ast.IntegerLiteral {
						if !c.inside_unsafe && !c.is_builtin_mod && field.default_expr.val == '0' {
							c.warn('default value of `0` for references can only be used inside `unsafe`',
								field.default_expr.pos)
						}
					}
					continue
				}
				if field.default_expr is ast.IntegerLiteral {
					if field.default_expr.val == '0' {
						c.warn('unnecessary default value of `0`: struct fields are zeroed by default',
							field.default_expr.pos)
					}
				} else if field.default_expr is ast.StringLiteral {
					if field.default_expr.val == '' {
						c.warn("unnecessary default value of '': struct fields are zeroed by default",
							field.default_expr.pos)
					}
				} else if field.default_expr is ast.BoolLiteral {
					if field.default_expr.val == false {
						c.warn('unnecessary default value `false`: struct fields are zeroed by default',
							field.default_expr.pos)
					}
				}
			}
		}
		if node.generic_types.len == 0 && has_generic_types {
			c.error('generic struct declaration must specify the generic type names, e.g. Foo<T>',
				node.pos)
		}
	}
}

fn minify_sort_fn(a &ast.StructField, b &ast.StructField) int {
	if a.typ == b.typ {
		return 0
	}
	// push all bool fields to the end of the struct
	if a.typ == ast.bool_type_idx {
		if b.typ == ast.bool_type_idx {
			return 0
		}
		return 1
	} else if b.typ == ast.bool_type_idx {
		return -1
	}

	mut t := global_table
	a_sym := t.sym(a.typ)
	b_sym := t.sym(b.typ)

	// push all non-flag enums to the end too, just before the bool fields
	// TODO: support enums with custom field values as well
	if a_sym.info is ast.Enum {
		if !a_sym.info.is_flag && !a_sym.info.uses_exprs {
			if b_sym.kind == .enum_ {
				a_nr_vals := (a_sym.info as ast.Enum).vals.len
				b_nr_vals := (b_sym.info as ast.Enum).vals.len
				return if a_nr_vals > b_nr_vals {
					-1
				} else if a_nr_vals < b_nr_vals {
					1
				} else {
					0
				}
			}
			return 1
		}
	} else if b_sym.info is ast.Enum {
		if !b_sym.info.is_flag && !b_sym.info.uses_exprs {
			return -1
		}
	}

	a_size, a_align := t.type_size(a.typ)
	b_size, b_align := t.type_size(b.typ)
	return if a_align > b_align {
		-1
	} else if a_align < b_align {
		1
	} else if a_size > b_size {
		-1
	} else if a_size < b_size {
		1
	} else {
		0
	}
}

pub fn (mut c Checker) struct_init(mut node ast.StructInit) ast.Type {
	if node.typ == ast.void_type {
		// short syntax `foo(key:val, key2:val2)`
		if c.expected_type == ast.void_type {
			c.error('unexpected short struct syntax', node.pos)
			return ast.void_type
		}
		sym := c.table.sym(c.expected_type)
		if sym.kind == .array {
			node.typ = c.table.value_type(c.expected_type)
		} else {
			node.typ = c.expected_type
		}
	}
	struct_sym := c.table.sym(node.typ)
	if struct_sym.info is ast.Struct {
		// check if the generic param types have been defined
		for ct in struct_sym.info.concrete_types {
			ct_sym := c.table.sym(ct)
			if ct_sym.kind == .placeholder {
				c.error('unknown type `$ct_sym.name`', node.pos)
			}
		}
		if struct_sym.info.generic_types.len > 0 && struct_sym.info.concrete_types.len == 0
			&& !node.is_short_syntax {
			if c.table.cur_concrete_types.len == 0 {
				c.error('generic struct init must specify type parameter, e.g. Foo<int>',
					node.pos)
			} else if node.generic_types.len == 0 {
				c.error('generic struct init must specify type parameter, e.g. Foo<T>',
					node.pos)
			} else if node.generic_types.len > 0
				&& node.generic_types.len != struct_sym.info.generic_types.len {
				c.error('generic struct init expects $struct_sym.info.generic_types.len generic parameter, but got $node.generic_types.len',
					node.pos)
			} else if node.generic_types.len > 0 && !isnil(c.table.cur_fn) {
				for gtyp in node.generic_types {
					gtyp_name := c.table.sym(gtyp).name
					if gtyp_name !in c.table.cur_fn.generic_names {
						cur_generic_names := '(' + c.table.cur_fn.generic_names.join(',') + ')'
						c.error('generic struct init type parameter `$gtyp_name` must be within the parameters `$cur_generic_names` of the current generic function',
							node.pos)
						break
					}
				}
			}
		}
		if node.generic_types.len > 0 && struct_sym.info.generic_types.len == node.generic_types.len
			&& struct_sym.info.generic_types != node.generic_types {
			c.table.replace_generic_type(node.typ, node.generic_types)
		}
	} else if struct_sym.info is ast.Alias {
		parent_sym := c.table.sym(struct_sym.info.parent_type)
		// e.g. ´x := MyMapAlias{}´, should be a cast to alias type ´x := MyMapAlias(map[...]...)´
		if parent_sym.kind == .map {
			alias_str := c.table.type_to_str(node.typ)
			map_str := c.table.type_to_str(struct_sym.info.parent_type)
			c.error('direct map alias init is not possible, use `${alias_str}($map_str{})` instead',
				node.pos)
			return ast.void_type
		}
	}
	// register generic struct type when current fn is generic fn
	if !isnil(c.table.cur_fn) && c.table.cur_fn.generic_names.len > 0 {
		c.table.unwrap_generic_type(node.typ, c.table.cur_fn.generic_names, c.table.cur_concrete_types)
	}
	c.ensure_type_exists(node.typ, node.pos) or {}
	type_sym := c.table.sym(node.typ)
	if !c.inside_unsafe && type_sym.kind == .sum_type {
		c.note('direct sum type init (`x := SumType{}`) will be removed soon', node.pos)
	}
	// Make sure the first letter is capital, do not allow e.g. `x := string{}`,
	// but `x := T{}` is ok.
	if !c.is_builtin_mod && !c.inside_unsafe && type_sym.language == .v
		&& c.table.cur_concrete_types.len == 0 {
		pos := type_sym.name.last_index('.') or { -1 }
		first_letter := type_sym.name[pos + 1]
		if !first_letter.is_capital() && type_sym.kind != .placeholder
			&& !type_sym.name.starts_with('main._VAnonStruct') {
			c.error('cannot initialize builtin type `$type_sym.name`', node.pos)
		}
	}
	if type_sym.kind == .sum_type && node.fields.len == 1 {
		sexpr := node.fields[0].expr.str()
		c.error('cast to sum type using `${type_sym.name}($sexpr)` not `$type_sym.name{$sexpr}`',
			node.pos)
	}
	if type_sym.kind == .interface_ && type_sym.language != .js {
		c.error('cannot instantiate interface `$type_sym.name`', node.pos)
	}
	if type_sym.info is ast.Alias {
		if type_sym.info.parent_type.is_number() {
			c.error('cannot instantiate number type alias `$type_sym.name`', node.pos)
			return ast.void_type
		}
	}
	// allow init structs from generic if they're private except the type is from builtin module
	if !type_sym.is_pub && type_sym.kind != .placeholder && type_sym.language != .c
		&& (type_sym.mod != c.mod && !(node.typ.has_flag(.generic) && type_sym.mod != 'builtin')) {
		c.error('type `$type_sym.name` is private', node.pos)
	}
	if type_sym.kind == .struct_ {
		info := type_sym.info as ast.Struct
		if info.attrs.len > 0 && info.attrs[0].name == 'noinit' && type_sym.mod != c.mod {
			c.error('struct `$type_sym.name` is declared with a `[noinit]` attribute, so ' +
				'it cannot be initialized with `$type_sym.name{}`', node.pos)
		}
	}
	if type_sym.name.len == 1 && !isnil(c.table.cur_fn) && c.table.cur_fn.generic_names.len == 0 {
		c.error('unknown struct `$type_sym.name`', node.pos)
		return 0
	}
	match type_sym.kind {
		.placeholder {
			c.error('unknown struct: $type_sym.name', node.pos)
			return ast.void_type
		}
		.any {
			// `T{ foo: 22 }`
			for mut field in node.fields {
				field.typ = c.expr(field.expr)
				field.expected_type = field.typ
			}
		}
		// string & array are also structs but .kind of string/array
		.struct_, .string, .array, .alias {
			mut info := ast.Struct{}
			if type_sym.kind == .alias {
				info_t := type_sym.info as ast.Alias
				sym := c.table.sym(info_t.parent_type)
				if sym.kind == .placeholder { // pending import symbol did not resolve
					c.error('unknown struct: $type_sym.name', node.pos)
					return ast.void_type
				}
				if sym.kind == .struct_ {
					info = sym.info as ast.Struct
				} else {
					c.error('alias type name: $sym.name is not struct type', node.pos)
				}
			} else {
				info = type_sym.info as ast.Struct
			}
			if node.no_keys {
				exp_len := info.fields.len
				got_len := node.fields.len
				if exp_len != got_len && !c.pref.translated {
					// XTODO remove !translated check
					amount := if exp_len < got_len { 'many' } else { 'few' }
					c.error('too $amount fields in `$type_sym.name` literal (expecting $exp_len, got $got_len)',
						node.pos)
				}
			}
			mut info_fields_sorted := []ast.StructField{}
			if node.no_keys {
				info_fields_sorted = info.fields.clone()
				info_fields_sorted.sort(a.i < b.i)
			}
			mut inited_fields := []string{}
			for i, mut field in node.fields {
				mut field_info := ast.StructField{}
				mut field_name := ''
				if node.no_keys {
					if i >= info.fields.len {
						// It doesn't make sense to check for fields that don't exist.
						// We should just stop here.
						break
					}
					field_info = info_fields_sorted[i]
					field_name = field_info.name
					node.fields[i].name = field_name
				} else {
					field_name = field.name
					mut exists := true
					field_info = c.table.find_field_with_embeds(type_sym, field_name) or {
						exists = false
						ast.StructField{}
					}
					if !exists {
						c.error('unknown field `$field.name` in struct literal of type `$type_sym.name`',
							field.pos)
						continue
					}
					if field_name in inited_fields {
						c.error('duplicate field name in struct literal: `$field_name`',
							field.pos)
						continue
					}
				}
				mut expr_type := ast.Type(0)
				mut expected_type := ast.Type(0)
				inited_fields << field_name
				field_type_sym := c.table.sym(field_info.typ)
				expected_type = field_info.typ
				c.expected_type = expected_type
				expr_type = c.expr(field.expr)
				if expr_type == ast.void_type {
					c.error('`$field.expr` (no value) used as value', field.pos)
				}
				if !field_info.typ.has_flag(.optional) {
					expr_type = c.check_expr_opt_call(field.expr, expr_type)
				}
				expr_type_sym := c.table.sym(expr_type)
				if field_type_sym.kind == .interface_ {
					if c.type_implements(expr_type, field_info.typ, field.pos) {
						if !expr_type.is_ptr() && !expr_type.is_pointer()
							&& expr_type_sym.kind != .interface_ && !c.inside_unsafe {
							c.mark_as_referenced(mut &field.expr, true)
						}
					}
				} else if expr_type != ast.void_type && expr_type_sym.kind != .placeholder {
					c.check_expected(c.unwrap_generic(expr_type), c.unwrap_generic(field_info.typ)) or {
						c.error('cannot assign to field `$field_info.name`: $err.msg()',
							field.pos)
					}
				}
				if field_info.typ.has_flag(.shared_f) {
					if !expr_type.has_flag(.shared_f) && expr_type.is_ptr() {
						c.error('`shared` field must be initialized with `shared` or value',
							field.pos)
					}
				} else {
					if field_info.typ.is_ptr() && !expr_type.is_ptr() && !expr_type.is_pointer()
						&& field.expr.str() != '0' {
						c.error('reference field must be initialized with reference',
							field.pos)
					}
				}
				if field_type_sym.kind == .function && field_type_sym.language == .v {
					pos := field.expr.pos()
					if mut field.expr is ast.AnonFn {
						if field.expr.decl.no_body {
							c.error('cannot initialize the fn field with anonymous fn that does not have a body',
								pos)
						}
					}
				}
				node.fields[i].typ = expr_type
				node.fields[i].expected_type = field_info.typ

				if field_info.typ.has_flag(.optional) {
					c.error('field `$field_info.name` is optional, but initialization of optional fields currently unsupported',
						field.pos)
				}
				if expr_type.is_ptr() && expected_type.is_ptr() {
					if mut field.expr is ast.Ident {
						if mut field.expr.obj is ast.Var {
							mut obj := unsafe { &field.expr.obj }
							if c.fn_scope != voidptr(0) {
								obj = c.fn_scope.find_var(obj.name) or { obj }
							}
							if obj.is_stack_obj && !c.inside_unsafe {
								sym := c.table.sym(obj.typ.set_nr_muls(0))
								if !sym.is_heap() && !c.pref.translated && !c.file.is_translated {
									suggestion := if sym.kind == .struct_ {
										'declaring `$sym.name` as `[heap]`'
									} else {
										'wrapping the `$sym.name` object in a `struct` declared as `[heap]`'
									}
									c.error('`$field.expr.name` cannot be assigned outside `unsafe` blocks as it might refer to an object stored on stack. Consider ${suggestion}.',
										field.expr.pos)
								}
							}
						}
					}
				}
			}
			// Check uninitialized refs/sum types
			for i, field in info.fields {
				if field.name in inited_fields {
					continue
				}
				if field.has_default_expr {
					if field.default_expr_typ == 0 {
						if field.default_expr is ast.StructInit {
							idx := c.table.find_type_idx(field.default_expr.typ_str)
							if idx != 0 {
								info.fields[i].default_expr_typ = ast.new_type(idx)
							}
						} else {
							if const_field := c.table.global_scope.find_const('$field.default_expr') {
								info.fields[i].default_expr_typ = const_field.typ
							}
						}
					}
					continue
				}
				if field.typ.is_ptr() && !field.typ.has_flag(.shared_f) && !node.has_update_expr
					&& !c.pref.translated && !c.file.is_translated {
					c.error('reference field `${type_sym.name}.$field.name` must be initialized',
						node.pos)
				}
				// Do not allow empty uninitialized interfaces
				sym := c.table.sym(field.typ)
				mut has_noinit := false
				for attr in field.attrs {
					if attr.name == 'noinit' {
						has_noinit = true
						break
					}
				}
				if sym.kind == .interface_ && (!has_noinit && sym.language != .js) {
					// TODO: should be an error instead, but first `ui` needs updating.
					c.note('interface field `${type_sym.name}.$field.name` must be initialized',
						node.pos)
				}
				// Do not allow empty uninitialized sum types
				/*
				sym := c.table.sym(field.typ)
				if sym.kind == .sum_type {
					c.warn('sum type field `${type_sym.name}.$field.name` must be initialized',
						node.pos)
				}
				*/
				// Check for `[required]` struct attr
				if field.attrs.contains('required') && !node.no_keys && !node.has_update_expr {
					mut found := false
					for init_field in node.fields {
						if field.name == init_field.name {
							found = true
							break
						}
					}
					if !found {
						c.error('field `${type_sym.name}.$field.name` must be initialized',
							node.pos)
					}
				}
			}
		}
		else {}
	}
	if node.has_update_expr {
		update_type := c.expr(node.update_expr)
		node.update_expr_type = update_type
		if c.table.type_kind(update_type) != .struct_ {
			s := c.table.type_to_str(update_type)
			c.error('expected struct, found `$s`', node.update_expr.pos())
		} else if update_type != node.typ {
			from_sym := c.table.sym(update_type)
			to_sym := c.table.sym(node.typ)
			from_info := from_sym.info as ast.Struct
			to_info := to_sym.info as ast.Struct
			// TODO this check is too strict
			if !c.check_struct_signature(from_info, to_info) {
				c.error('struct `$from_sym.name` is not compatible with struct `$to_sym.name`',
					node.update_expr.pos())
			}
		}
		if !node.update_expr.is_lvalue() {
			// cgen will repeat `update_expr` for each field
			// so enforce an lvalue for efficiency
			c.error('expression is not an lvalue', node.update_expr.pos())
		}
	}
	return node.typ
}
