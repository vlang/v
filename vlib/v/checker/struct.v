// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import v.util
import v.token

fn (mut c Checker) struct_decl(mut node ast.StructDecl) {
	util.timing_start(@METHOD)
	defer {
		util.timing_measure_cumulative(@METHOD)
	}
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
				c.error('`${embed_sym.name}` is not a struct', embed.pos)
			} else {
				info := embed_sym.info as ast.Struct
				if info.is_heap && !embed.typ.is_ptr() {
					struct_sym.info.is_heap = true
				}
			}
			// Ensure each generic type of the embed was declared in the struct's definition
			if node.generic_types.len > 0 && embed.typ.has_flag(.generic) {
				embed_generic_names := c.table.generic_type_names(embed.typ)
				node_generic_names := node.generic_types.map(c.table.type_to_str(it))
				for name in embed_generic_names {
					if name !in node_generic_names {
						struct_generic_names := node_generic_names.join(', ')
						c.error('generic type name `${name}` is not mentioned in struct `${node.name}[${struct_generic_names}]`',
							embed.pos)
					}
				}
			}
		}
		if struct_sym.info.is_minify && !c.pref.output_cross_c {
			node.fields.sort_with_compare(minify_sort_fn)
			struct_sym.info.fields.sort_with_compare(minify_sort_fn)
		}
		for attr in node.attrs {
			if node.language != .c && attr.name == 'typedef' {
				c.error('`typedef` attribute can only be used with C structs', node.pos)
			}
		}

		// Evaluate the size of the unresolved fixed array
		for mut field in node.fields {
			sym := c.table.sym(field.typ)
			if sym.info is ast.ArrayFixed && c.array_fixed_has_unresolved_size(sym.info) {
				mut size_expr := unsafe { sym.info.size_expr }
				field.typ = c.eval_array_fixed_sizes(mut size_expr, 0, sym.info.elem_type)
				for mut symfield in struct_sym.info.fields {
					if symfield.name == field.name {
						symfield.typ = field.typ
					}
				}
			}
		}

		// Update .default_expr_typ for all fields in the struct:
		util.timing_start('Checker.struct setting default_expr_typ')
		old_expected_type := c.expected_type
		for mut field in node.fields {
			// when the field has the same type that the struct itself (recursive)
			if field.typ.clear_flag(.option).set_nr_muls(0) == struct_typ_idx {
				for mut symfield in struct_sym.info.fields {
					if symfield.name == field.name {
						// only ?&Struct is allowed to be recursive
						if field.typ.is_ptr() {
							symfield.is_recursive = true
						} else {
							c.error('recursive struct is only possible with optional pointer (e.g. ?&${c.table.type_to_str(field.typ.clear_flag(.option))})',
								node.pos)
						}
					}
				}
			}
			// Do not allow uninitialized `fn` fields, or force `?fn`
			// (allow them in `C.` structs)
			if !c.is_builtin_mod && node.language == .v {
				sym := c.table.sym(field.typ)
				if sym.kind == .function {
					if !field.typ.has_flag(.option) && !field.has_default_expr
						&& field.attrs.all(it.name != 'required') {
						error_msg := 'uninitialized `fn` struct fields are not allowed, since they can result in segfaults; use `?fn` or `[required]` or initialize the field with `=` (if you absolutely want to have unsafe function pointers, use `= unsafe { nil }`)'
						c.note(error_msg, field.pos)
					}
				}
			}

			if field.has_default_expr {
				c.expected_type = field.typ
				field.default_expr_typ = c.expr(mut field.default_expr)
				for mut symfield in struct_sym.info.fields {
					if symfield.name == field.name {
						symfield.default_expr_typ = field.default_expr_typ
						break
					}
				}
			}
			// check anon struct declaration
			if field.anon_struct_decl.fields.len > 0 {
				c.struct_decl(mut field.anon_struct_decl)
			}
		}
		c.expected_type = old_expected_type
		util.timing_measure_cumulative('Checker.struct setting default_expr_typ')

		for i, field in node.fields {
			if field.typ.has_flag(.result) {
				c.error('struct field does not support storing Result', field.option_pos)
			}
			if !c.ensure_type_exists(field.typ, field.type_pos) {
				continue
			}
			if !c.ensure_generic_type_specify_type_names(field.typ, field.type_pos) {
				continue
			}
			if field.typ.has_flag(.generic) {
				has_generic_types = true
			}
			if node.language == .v {
				c.check_valid_snake_case(field.name, 'field name', field.pos)
			}
			sym := c.table.sym(field.typ)
			for j in 0 .. i {
				if field.name == node.fields[j].name {
					c.error('field name `${field.name}` duplicate', field.pos)
				}
			}
			if field.typ != 0 {
				if !field.typ.is_ptr() {
					if c.table.unaliased_type(field.typ) == struct_typ_idx {
						c.error('field `${field.name}` is part of `${node.name}`, they can not both have the same type',
							field.type_pos)
					}
				}
			}
			match sym.kind {
				.struct_ {
					info := sym.info as ast.Struct
					if info.is_heap && !field.typ.is_ptr() {
						struct_sym.info.is_heap = true
					}
					for ct in info.concrete_types {
						ct_sym := c.table.sym(ct)
						if ct_sym.kind == .placeholder {
							c.error('unknown type `${ct_sym.name}`', field.type_pos)
						}
					}
				}
				.multi_return {
					c.error('cannot use multi return as field type', field.type_pos)
				}
				.none_ {
					c.error('cannot use `none` as field type', field.type_pos)
				}
				.map {
					info := sym.map_info()
					if info.value_type.has_flag(.result) {
						c.error('cannot use Result type as map value type', field.type_pos)
					}
				}
				.alias {
					if sym.name == 'byte' {
						c.warn('byte is deprecated, use u8 instead', field.type_pos)
					}
				}
				else {}
			}

			if field.has_default_expr {
				c.expected_type = field.typ
				if !field.typ.has_flag(.option) && !field.typ.has_flag(.result) {
					c.check_expr_option_or_result_call(field.default_expr, field.default_expr_typ)
				}
				interface_implemented := sym.kind == .interface_
					&& c.type_implements(field.default_expr_typ, field.typ, field.pos)
				c.check_expected(field.default_expr_typ, field.typ) or {
					if sym.kind == .interface_ && interface_implemented {
						if !c.inside_unsafe && !field.default_expr_typ.is_any_kind_of_pointer() {
							if c.table.sym(field.default_expr_typ).kind != .interface_ {
								c.mark_as_referenced(mut &node.fields[i].default_expr,
									true)
							}
						}
					} else if c.table.final_sym(field.typ).kind == .function
						&& field.default_expr_typ.is_pointer() {
						continue
					} else {
						c.error('incompatible initializer for field `${field.name}`: ${err.msg()}',
							field.default_expr.pos())
					}
				}
				if field.default_expr.is_nil() {
					if !field.typ.is_any_kind_of_pointer()
						&& c.table.sym(field.typ).kind != .function {
						c.error('cannot assign `nil` to a non-pointer field', field.type_pos)
					}
				}
				// Check for unnecessary inits like ` = 0` and ` = ''`
				if field.typ.is_ptr() {
					if field.default_expr is ast.IntegerLiteral {
						if !c.inside_unsafe && !c.is_builtin_mod && field.default_expr.val == '0' {
							c.error('default value of `0` for references can only be used inside `unsafe`',
								field.default_expr.pos)
						}
					}
					if field.typ.has_flag(.option) && field.default_expr is ast.None {
						c.warn('unnecessary default value of `none`: struct fields are zeroed by default',
							field.default_expr.pos)
					}
					if field.typ.has_flag(.option) && field.default_expr.is_nil() {
						c.error('cannot assign `nil` to option value', field.default_expr.pos())
					}
					continue
				}
				if field.typ in ast.unsigned_integer_type_idxs {
					if field.default_expr is ast.IntegerLiteral {
						if field.default_expr.val[0] == `-` {
							c.error('cannot assign negative value to unsigned integer type',
								field.default_expr.pos)
						}
					}
				}
				if field.typ.has_flag(.option) {
					if field.default_expr is ast.None {
						c.warn('unnecessary default value of `none`: struct fields are zeroed by default',
							field.default_expr.pos)
					}
				} else if field.typ.has_flag(.result) {
					// struct field does not support result. Nothing to do
				} else {
					match field.default_expr {
						ast.IntegerLiteral {
							if field.default_expr.val == '0' {
								c.warn('unnecessary default value of `0`: struct fields are zeroed by default',
									field.default_expr.pos)
							}
						}
						ast.StringLiteral {
							if field.default_expr.val == '' {
								c.warn("unnecessary default value of '': struct fields are zeroed by default",
									field.default_expr.pos)
							}
						}
						ast.BoolLiteral {
							if field.default_expr.val == false {
								c.warn('unnecessary default value `false`: struct fields are zeroed by default',
									field.default_expr.pos)
							}
						}
						else {}
					}
				}
			}
			// Ensure each generic type of the field was declared in the struct's definition
			if node.generic_types.len > 0 && field.typ.has_flag(.generic) {
				field_generic_names := c.table.generic_type_names(field.typ)
				node_generic_names := node.generic_types.map(c.table.type_to_str(it))
				for name in field_generic_names {
					if name !in node_generic_names {
						struct_generic_names := node_generic_names.join(', ')
						c.error('generic type name `${name}` is not mentioned in struct `${node.name}[${struct_generic_names}]`',
							field.type_pos)
					}
				}
			}
		}
		if node.generic_types.len == 0 && has_generic_types {
			c.error('generic struct `${node.name}` declaration must specify the generic type names, e.g. ${node.name}[T]',
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

fn (mut c Checker) struct_init(mut node ast.StructInit, is_field_zero_struct_init bool, mut inited_fields []string) ast.Type {
	util.timing_start(@METHOD)
	defer {
		util.timing_measure_cumulative(@METHOD)
	}
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
	mut old_inside_generic_struct_init := false
	mut old_cur_struct_generic_types := []ast.Type{}
	mut old_cur_struct_concrete_types := []ast.Type{}
	if struct_sym.info is ast.Struct {
		// check if the generic param types have been defined
		for ct in struct_sym.info.concrete_types {
			ct_sym := c.table.sym(ct)
			if ct_sym.kind == .placeholder {
				c.error('unknown type `${ct_sym.name}`', node.pos)
			}
		}
		if struct_sym.info.generic_types.len > 0 && struct_sym.info.concrete_types.len == 0
			&& !node.is_short_syntax && c.table.cur_concrete_types.len != 0
			&& !is_field_zero_struct_init {
			if node.generic_types.len == 0 {
				c.error('generic struct init must specify type parameter, e.g. Foo[T]',
					node.pos)
			} else if node.generic_types.len > 0
				&& node.generic_types.len != struct_sym.info.generic_types.len {
				c.error('generic struct init expects ${struct_sym.info.generic_types.len} generic parameter, but got ${node.generic_types.len}',
					node.pos)
			} else if node.generic_types.len > 0 && c.table.cur_fn != unsafe { nil } {
				for gtyp in node.generic_types {
					if !gtyp.has_flag(.generic) {
						continue
					}
					gtyp_name := c.table.sym(gtyp).name
					if gtyp_name.len == 1 && gtyp_name !in c.table.cur_fn.generic_names {
						cur_generic_names := '(' + c.table.cur_fn.generic_names.join(',') + ')'
						c.error('generic struct init type parameter `${gtyp_name}` must be within the parameters `${cur_generic_names}` of the current generic function',
							node.pos)
						break
					}
				}
			}
		}
		if node.generic_types.len > 0 && struct_sym.info.generic_types.len == 0 {
			c.error('a non generic struct `${node.typ_str}` used like a generic struct',
				node.name_pos)
		}
		if struct_sym.info.generic_types.len > 0
			&& struct_sym.info.generic_types.len == struct_sym.info.concrete_types.len {
			old_inside_generic_struct_init = c.inside_generic_struct_init
			old_cur_struct_generic_types = c.cur_struct_generic_types.clone()
			old_cur_struct_concrete_types = c.cur_struct_concrete_types.clone()
			c.inside_generic_struct_init = true
			c.cur_struct_generic_types = struct_sym.info.generic_types.clone()
			c.cur_struct_concrete_types = struct_sym.info.concrete_types.clone()
			defer {
				c.inside_generic_struct_init = old_inside_generic_struct_init
				c.cur_struct_generic_types = old_cur_struct_generic_types
				c.cur_struct_concrete_types = old_cur_struct_concrete_types
			}
		}
	} else if struct_sym.info is ast.Alias {
		parent_sym := c.table.sym(struct_sym.info.parent_type)
		// e.g. ´x := MyMapAlias{}´, should be a cast to alias type ´x := MyMapAlias(map[...]...)´
		if parent_sym.kind == .map {
			alias_str := c.table.type_to_str(node.typ)
			map_str := c.table.type_to_str(struct_sym.info.parent_type)
			c.error('direct map alias init is not possible, use `${alias_str}(${map_str}{})` instead',
				node.pos)
			return ast.void_type
		}
	} else if struct_sym.info is ast.FnType {
		c.error('functions must be defined, not instantiated like structs', node.pos)
	}
	// register generic struct type when current fn is generic fn
	if c.table.cur_fn != unsafe { nil } && c.table.cur_fn.generic_names.len > 0 {
		c.table.unwrap_generic_type(node.typ, c.table.cur_fn.generic_names, c.table.cur_concrete_types)
	}
	if !is_field_zero_struct_init {
		c.ensure_type_exists(node.typ, node.pos)
	}
	type_sym := c.table.sym(node.typ)
	if !c.inside_unsafe && type_sym.kind == .sum_type {
		c.note('direct sum type init (`x := SumType{}`) will be removed soon', node.pos)
	}
	// Make sure the first letter is capital, do not allow e.g. `x := string{}`,
	// but `x := T{}` is ok.
	if !c.is_builtin_mod && !c.inside_unsafe && type_sym.language == .v
		&& c.table.cur_concrete_types.len == 0 {
		pos := type_sym.name.index_u8_last(`.`)
		first_letter := type_sym.name[pos + 1]
		if !first_letter.is_capital()
			&& (type_sym.kind != .struct_ || !(type_sym.info as ast.Struct).is_anon)
			&& type_sym.kind != .placeholder {
			c.error('cannot initialize builtin type `${type_sym.name}`', node.pos)
		}
		if type_sym.kind == .enum_ && !c.pref.translated && !c.file.is_translated {
			c.error('cannot initialize enums', node.pos)
		}
	}
	if type_sym.kind == .sum_type && node.init_fields.len == 1 {
		sexpr := node.init_fields[0].expr.str()
		c.error('cast to sum type using `${type_sym.name}(${sexpr})` not `${type_sym.name}{${sexpr}}`',
			node.pos)
	}
	if type_sym.kind == .interface_ && type_sym.language != .js {
		c.error('cannot instantiate interface `${type_sym.name}`', node.pos)
	}
	if type_sym.info is ast.Alias {
		if type_sym.info.parent_type.is_number() {
			c.error('cannot instantiate number type alias `${type_sym.name}`', node.pos)
			return ast.void_type
		}
	}
	// allow init structs from generic if they're private except the type is from builtin module
	if !node.has_update_expr && !type_sym.is_pub && type_sym.kind != .placeholder
		&& type_sym.language != .c && (type_sym.mod != c.mod && !(node.typ.has_flag(.generic)
		&& type_sym.mod != 'builtin')) && !is_field_zero_struct_init {
		c.error('type `${type_sym.name}` is private', node.pos)
	}
	if type_sym.kind == .struct_ {
		info := type_sym.info as ast.Struct
		if info.attrs.len > 0 && info.attrs.contains('noinit') && type_sym.mod != c.mod {
			c.error('struct `${type_sym.name}` is declared with a `@[noinit]` attribute, so ' +
				'it cannot be initialized with `${type_sym.name}{}`', node.pos)
		}
	}
	if type_sym.name.len == 1 && c.table.cur_fn != unsafe { nil }
		&& c.table.cur_fn.generic_names.len == 0 {
		c.error('unknown struct `${type_sym.name}`', node.pos)
		return ast.void_type
	}
	match type_sym.kind {
		.placeholder {
			c.error('unknown struct: ${type_sym.name}', node.pos)
			return ast.void_type
		}
		.any {
			// `T{ foo: 22 }`
			for mut init_field in node.init_fields {
				init_field.typ = c.expr(mut init_field.expr)
				init_field.expected_type = init_field.typ
			}
			sym := c.table.sym(c.unwrap_generic(node.typ))
			if sym.kind == .struct_ {
				info := sym.info as ast.Struct
				if info.attrs.len > 0 && info.attrs.contains('noinit') && sym.mod != c.mod {
					c.error('struct `${sym.name}` is declared with a `@[noinit]` attribute, so ' +
						'it cannot be initialized with `${sym.name}{}`', node.pos)
				}
				if node.no_keys && node.init_fields.len != info.fields.len {
					fname := if info.fields.len != 1 { 'fields' } else { 'field' }
					c.error('initializing struct `${sym.name}` needs `${info.fields.len}` ${fname}, but got `${node.init_fields.len}`',
						node.pos)
				}
			}
		}
		// string & array are also structs but .kind of string/array
		.struct_, .string, .array, .alias {
			mut info := ast.Struct{}
			if type_sym.kind == .alias {
				info_t := type_sym.info as ast.Alias
				sym := c.table.sym(info_t.parent_type)
				if sym.kind == .placeholder { // pending import symbol did not resolve
					c.error('unknown struct: ${type_sym.name}', node.pos)
					return ast.void_type
				}
				if sym.kind == .struct_ {
					info = sym.info as ast.Struct
				} else {
					c.error('alias type name: ${sym.name} is not struct type', node.pos)
				}
			} else {
				info = type_sym.info as ast.Struct
			}
			if node.no_keys {
				exp_len := info.fields.len
				got_len := node.init_fields.len
				if exp_len != got_len && !c.pref.translated {
					// XTODO remove !translated check
					amount := if exp_len < got_len { 'many' } else { 'few' }
					c.error('too ${amount} fields in `${type_sym.name}` literal (expecting ${exp_len}, got ${got_len})',
						node.pos)
				}
			}
			mut info_fields_sorted := []ast.StructField{}
			if node.no_keys {
				info_fields_sorted = info.fields.clone()
				info_fields_sorted.sort(a.i < b.i)
			}
			for i, mut init_field in node.init_fields {
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
					node.init_fields[i].name = field_name
				} else {
					field_name = init_field.name
					mut exists := true
					field_info = c.table.find_field_with_embeds(type_sym, field_name) or {
						exists = false
						ast.StructField{}
					}
					if !exists {
						existing_fields := c.table.struct_fields(type_sym).map(it.name)
						c.error(util.new_suggestion(init_field.name, existing_fields).say('unknown field `${init_field.name}` in struct literal of type `${type_sym.name}`'),
							init_field.pos)
						continue
					}
					if field_name in inited_fields {
						c.error('duplicate field name in struct literal: `${field_name}`',
							init_field.pos)
						continue
					}
				}
				mut got_type := ast.Type(0)
				mut exp_type := ast.Type(0)
				inited_fields << field_name
				exp_type = field_info.typ
				exp_type_sym := c.table.sym(exp_type)
				c.expected_type = exp_type
				got_type = c.expr(mut init_field.expr)
				got_type_sym := c.table.sym(got_type)
				if got_type == ast.void_type {
					c.error('`${init_field.expr}` (no value) used as value', init_field.pos)
				}
				if !exp_type.has_flag(.option) {
					got_type = c.check_expr_option_or_result_call(init_field.expr, got_type)
					if got_type.has_flag(.option) {
						c.error('cannot assign an Option value to a non-option struct field',
							init_field.pos)
					} else if got_type.has_flag(.result) {
						c.error('cannot assign a Result value to a non-option struct field',
							init_field.pos)
					}
				}
				if got_type.has_flag(.result) {
					c.check_expr_option_or_result_call(init_field.expr, init_field.typ)
				}
				if exp_type.has_flag(.option) && got_type.is_ptr() && !(exp_type.is_ptr()
					&& exp_type_sym.kind == .struct_) {
					c.error('cannot assign a pointer to option struct field', init_field.pos)
				}
				if exp_type_sym.kind == .voidptr && got_type_sym.kind == .struct_
					&& !got_type.is_ptr() {
					c.error('allocate `${got_type_sym.name}` on the heap for use in other functions',
						init_field.pos)
				}
				if exp_type_sym.kind == .array && got_type_sym.kind == .array {
					if init_field.expr is ast.IndexExpr
						&& (init_field.expr as ast.IndexExpr).left is ast.Ident
						&& ((init_field.expr as ast.IndexExpr).left.is_mut()
						|| field_info.is_mut) && init_field.expr.index is ast.RangeExpr
						&& !c.inside_unsafe {
						// `a: arr[..]` auto add clone() -> `a: arr[..].clone()`
						c.add_error_detail_with_pos('To silence this notice, use either an explicit `a[..].clone()`,
or use an explicit `unsafe{ a[..] }`, if you do not want a copy of the slice.',
							init_field.expr.pos())
						c.note('an implicit clone of the slice was done here', init_field.expr.pos())
						mut right := ast.CallExpr{
							name: 'clone'
							left: init_field.expr
							left_type: got_type
							is_method: true
							receiver_type: got_type.ref()
							return_type: got_type
							scope: c.fn_scope
						}
						got_type = c.expr(mut right)
						node.init_fields[i].expr = right
					}
				}
				if exp_type_sym.kind == .interface_ {
					if c.type_implements(got_type, exp_type, init_field.pos) {
						if !c.inside_unsafe && got_type_sym.kind != .interface_
							&& !got_type.is_any_kind_of_pointer() {
							c.mark_as_referenced(mut &init_field.expr, true)
						}
					}
				} else if got_type != ast.void_type && got_type_sym.kind != .placeholder
					&& !exp_type.has_flag(.generic) {
					c.check_expected(c.unwrap_generic(got_type), c.unwrap_generic(exp_type)) or {
						c.error('cannot assign to field `${field_info.name}`: ${err.msg()}',
							init_field.pos)
					}
				}
				if exp_type.has_flag(.shared_f) {
					if !got_type.has_flag(.shared_f) && got_type.is_ptr() {
						c.error('`shared` field must be initialized with `shared` or value',
							init_field.pos)
					}
				} else {
					is_unsafe_0 := init_field.expr is ast.UnsafeExpr
						&& (init_field.expr as ast.UnsafeExpr).expr.str() == '0'
					if exp_type.is_ptr() && !is_unsafe_0 && !got_type.is_any_kind_of_pointer()
						&& !exp_type.has_flag(.option) {
						if init_field.expr.str() == '0' {
							if !c.inside_unsafe && type_sym.language == .v {
								c.note('assigning `0` to a reference field is only allowed in `unsafe` blocks',
									init_field.pos)
							}
						} else {
							c.error('reference field must be initialized with reference',
								init_field.pos)
						}
					} else if exp_type.is_any_kind_of_pointer()
						&& !got_type.is_any_kind_of_pointer() && !got_type.is_int()
						&& (!exp_type.has_flag(.option) || got_type.idx() != ast.none_type_idx) {
						got_typ_str := c.table.type_to_str(got_type)
						exp_typ_str := c.table.type_to_str(exp_type)
						c.error('cannot assign to field `${field_info.name}`: expected a pointer `${exp_typ_str}`, but got `${got_typ_str}`',
							init_field.pos)
					}
				}
				node.init_fields[i].typ = got_type
				node.init_fields[i].expected_type = exp_type

				if got_type.is_ptr() && exp_type.is_ptr() {
					if mut init_field.expr is ast.Ident {
						c.fail_if_stack_struct_action_outside_unsafe(mut init_field.expr,
							'assigned')
					}
				}
				if field_info.typ in ast.unsigned_integer_type_idxs {
					if mut init_field.expr is ast.IntegerLiteral {
						if init_field.expr.val[0] == `-` {
							c.error('cannot assign negative value to unsigned integer type',
								init_field.expr.pos)
						}
					}
				}

				if exp_type_sym.kind == .struct_ && !(exp_type_sym.info as ast.Struct).is_anon
					&& mut init_field.expr is ast.StructInit {
					if init_field.expr.is_anon {
						c.error('cannot assign anonymous `struct` to a typed `struct`',
							init_field.expr.pos)
					}
				}

				// all the fields of initialized embedded struct are ignored, they are considered initialized
				sym := c.table.sym(init_field.typ)
				if init_field.name.len > 0 && init_field.name[0].is_capital()
					&& sym.kind == .struct_ && sym.language == .v {
					struct_fields := c.table.struct_fields(sym)
					for struct_field in struct_fields {
						inited_fields << struct_field.name
					}
				}
				expected_type_sym := c.table.final_sym(init_field.expected_type)
				if expected_type_sym.kind in [.string, .array, .map, .array_fixed, .chan, .struct_]
					&& init_field.expr.is_nil() && !init_field.expected_type.is_ptr()
					&& mut init_field.expr is ast.UnsafeExpr {
					c.error('cannot assign `nil` to struct field `${init_field.name}` with type `${expected_type_sym.name}`',
						init_field.expr.pos.extend(init_field.expr.expr.pos()))
				}
			}
			// Check uninitialized refs/sum types
			// The variable `fields` contains two parts, the first part is the same as info.fields,
			// and the second part is all fields embedded in the structure
			// If the return value data composition form in `c.table.struct_fields()` is modified,
			// need to modify here accordingly.
			mut fields := c.table.struct_fields(type_sym)
			mut checked_types := []ast.Type{}

			for i, mut field in fields {
				if field.name in inited_fields {
					continue
				}
				sym := c.table.sym(field.typ)
				if field.name.len > 0 && field.name[0].is_capital() && sym.info is ast.Struct
					&& sym.language == .v {
					// struct embeds
					continue
				}
				if field.has_default_expr {
					if i < info.fields.len && field.default_expr_typ == 0 {
						if mut field.default_expr is ast.StructInit {
							idx := c.table.find_type_idx(field.default_expr.typ_str)
							if idx != 0 {
								info.fields[i].default_expr_typ = ast.new_type(idx)
							}
						} else if field.default_expr.is_nil() {
							if field.typ.is_any_kind_of_pointer() {
								info.fields[i].default_expr_typ = field.typ
							}
						} else if field.default_expr is ast.Ident
							&& field.default_expr.info is ast.IdentFn {
							c.expr(mut field.default_expr)
						} else {
							if const_field := c.table.global_scope.find_const('${field.default_expr}') {
								info.fields[i].default_expr_typ = const_field.typ
							} else if type_sym.info is ast.Struct && type_sym.info.is_anon {
								c.expected_type = field.typ
								field.default_expr_typ = c.expr(mut field.default_expr)
								info.fields[i].default_expr_typ = field.default_expr_typ
							}
						}
					}
					continue
				}
				if field.typ.is_ptr() && !field.typ.has_flag(.shared_f)
					&& !field.typ.has_flag(.option) && !node.has_update_expr && !c.pref.translated
					&& !c.file.is_translated {
					c.error('reference field `${type_sym.name}.${field.name}` must be initialized',
						node.pos)
					continue
				}
				if !field.typ.has_flag(.option) {
					if sym.kind == .struct_ {
						c.check_ref_fields_initialized(sym, mut checked_types, '${type_sym.name}.${field.name}',
							node.pos)
					} else if sym.kind == .alias {
						parent_sym := c.table.sym((sym.info as ast.Alias).parent_type)
						if parent_sym.kind == .struct_ {
							c.check_ref_fields_initialized(parent_sym, mut checked_types,
								'${type_sym.name}.${field.name}', node.pos)
						}
					}
				}
				// Do not allow empty uninitialized interfaces
				mut has_noinit := false
				for attr in field.attrs {
					if attr.name == 'noinit' {
						has_noinit = true
						break
					}
				}
				if !field.typ.has_flag(.option) && sym.kind == .interface_
					&& (!has_noinit && sym.language != .js) && !node.has_update_expr {
					// TODO: should be an error instead, but first `ui` needs updating.
					c.note('interface field `${type_sym.name}.${field.name}` must be initialized',
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
				if !node.no_keys && !node.has_update_expr && field.attrs.contains('required') {
					mut found := false
					for init_field in node.init_fields {
						if field.name == init_field.name {
							found = true
							break
						}
					}
					if !found {
						c.error('field `${type_sym.name}.${field.name}` must be initialized',
							node.pos)
					}
				}
				if !node.has_update_expr && !field.has_default_expr && field.name !in inited_fields
					&& !field.typ.is_ptr() && !field.typ.has_flag(.option)
					&& c.table.final_sym(field.typ).kind == .struct_ {
					mut zero_struct_init := ast.StructInit{
						pos: node.pos
						typ: field.typ
					}
					c.struct_init(mut zero_struct_init, true, mut inited_fields)
				}
			}
			for embed in info.embeds {
				mut zero_struct_init := ast.StructInit{
					pos: node.pos
					typ: embed
				}
				c.struct_init(mut zero_struct_init, true, mut inited_fields)
			}
			// println('>> checked_types.len: $checked_types.len | checked_types: $checked_types | type_sym: $type_sym.name ')
		}
		else {}
	}
	if node.has_update_expr {
		update_type := c.expr(mut node.update_expr)
		node.update_expr_type = update_type
		expr_sym := c.table.final_sym(c.unwrap_generic(update_type))
		if node.update_expr is ast.ComptimeSelector {
			c.error('cannot use struct update syntax in compile time expressions', node.update_expr_pos)
		} else if expr_sym.kind != .struct_ {
			s := c.table.type_to_str(update_type)
			c.error('expected struct, found `${s}`', node.update_expr.pos())
		} else if update_type != node.typ {
			from_sym := c.table.sym(update_type)
			to_sym := c.table.sym(node.typ)
			from_info := from_sym.info as ast.Struct
			to_info := to_sym.info as ast.Struct
			// TODO this check is too strict
			if !c.check_struct_signature(from_info, to_info)
				|| !c.check_struct_signature_init_fields(from_info, to_info, node) {
				c.error('struct `${from_sym.name}` is not compatible with struct `${to_sym.name}`',
					node.update_expr.pos())
			}
		}
	}
	if struct_sym.info is ast.Struct {
		if struct_sym.info.generic_types.len > 0 && struct_sym.info.concrete_types.len == 0
			&& c.table.cur_concrete_types.len == 0 {
			concrete_types := c.infer_struct_generic_types(node.typ, node)
			if concrete_types.len > 0 {
				generic_names := struct_sym.info.generic_types.map(c.table.sym(it).name)
				node.typ = c.table.unwrap_generic_type(node.typ, generic_names, concrete_types)
			}
		} else if struct_sym.info.generic_types.len > 0
			&& struct_sym.info.generic_types.len == struct_sym.info.concrete_types.len
			&& c.table.cur_concrete_types.len == 0 {
			parent_type := struct_sym.info.parent_type
			parent_sym := c.table.sym(parent_type)
			for method in parent_sym.methods {
				generic_names := struct_sym.info.generic_types.map(c.table.sym(it).name)
				for i, param in method.params {
					if i == 0 || !param.typ.has_flag(.generic) {
						continue
					}
					param_sym := c.table.sym(param.typ)
					if param_sym.kind in [.struct_, .interface_, .sum_type] {
						c.table.unwrap_generic_type(param.typ, generic_names, struct_sym.info.concrete_types)
					}
				}
			}
		}
	}
	return node.typ
}

// Recursively check whether the struct type field is initialized
fn (mut c Checker) check_ref_fields_initialized(struct_sym &ast.TypeSymbol, mut checked_types []ast.Type, linked_name string, pos &token.Pos) {
	if c.pref.translated || c.file.is_translated {
		return
	}
	if struct_sym.kind == .struct_ && struct_sym.language == .c
		&& (struct_sym.info as ast.Struct).is_typedef {
		return
	}
	fields := c.table.struct_fields(struct_sym)
	for field in fields {
		sym := c.table.sym(field.typ)
		if field.name.len > 0 && field.name[0].is_capital() && sym.info is ast.Struct
			&& sym.language == .v {
			// an embedded struct field
			continue
		}
		if field.typ.is_ptr() && !field.typ.has_flag(.shared_f) && !field.typ.has_flag(.option)
			&& !field.has_default_expr {
			c.error('reference field `${linked_name}.${field.name}` must be initialized (part of struct `${struct_sym.name}`)',
				pos)
			continue
		}
		if sym.kind == .struct_ {
			if sym.language == .c && (sym.info as ast.Struct).is_typedef {
				continue
			}
			if field.typ in checked_types {
				continue
			}
			checked_types << field.typ
			c.check_ref_fields_initialized(sym, mut checked_types, '${linked_name}.${field.name}',
				pos)
		} else if sym.kind == .alias {
			psym := c.table.sym((sym.info as ast.Alias).parent_type)
			if psym.kind == .struct_ {
				checked_types << field.typ
				c.check_ref_fields_initialized(psym, mut checked_types, '${linked_name}.${field.name}',
					pos)
			}
		}
	}
}

// Recursively check whether the struct type field is initialized
// NOTE:
// This method is temporary and will only be called by the do_check_elements_ref_fields_initialized() method.
// The goal is to give only a notice, not an error, for now. After a while,
// when we change the notice to error, we can remove this temporary method.
fn (mut c Checker) check_ref_fields_initialized_note(struct_sym &ast.TypeSymbol, mut checked_types []ast.Type, linked_name string, pos &token.Pos) {
	if c.pref.translated || c.file.is_translated {
		return
	}
	if struct_sym.kind == .struct_ && struct_sym.language == .c
		&& (struct_sym.info as ast.Struct).is_typedef {
		return
	}
	fields := c.table.struct_fields(struct_sym)
	for field in fields {
		sym := c.table.sym(field.typ)
		if field.name.len > 0 && field.name[0].is_capital() && sym.info is ast.Struct
			&& sym.language == .v {
			// an embedded struct field
			continue
		}
		if field.typ.is_ptr() && !field.typ.has_flag(.shared_f) && !field.typ.has_flag(.option)
			&& !field.has_default_expr {
			c.note('reference field `${linked_name}.${field.name}` must be initialized (part of struct `${struct_sym.name}`)',
				pos)
			continue
		}
		if sym.kind == .struct_ {
			if sym.language == .c && (sym.info as ast.Struct).is_typedef {
				continue
			}
			if field.typ in checked_types {
				continue
			}
			checked_types << field.typ
			c.check_ref_fields_initialized(sym, mut checked_types, '${linked_name}.${field.name}',
				pos)
		} else if sym.kind == .alias {
			psym := c.table.sym((sym.info as ast.Alias).parent_type)
			if psym.kind == .struct_ {
				checked_types << field.typ
				c.check_ref_fields_initialized(psym, mut checked_types, '${linked_name}.${field.name}',
					pos)
			}
		}
	}
}
