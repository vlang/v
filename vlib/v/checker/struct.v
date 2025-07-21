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
	if node.language in [.c, .js] && node.generic_types.len > 0 {
		lang := if node.language == .c { 'C' } else { 'JS' }
		c.error('${lang} structs cannot be declared as generic', node.pos)
	}
	node_name := if node.scoped_name != '' { node.scoped_name } else { node.name }
	mut struct_sym, struct_typ_idx := c.table.find_sym_and_type_idx(node_name)
	node.idx = struct_typ_idx
	mut has_generic_types := false
	if mut struct_sym.info is ast.Struct {
		for mut symfield in struct_sym.info.fields {
			symfield.container_typ = struct_typ_idx
			if struct_sym.info.is_union {
				symfield.is_part_of_union = true
			}
		}

		if node.language == .v && !c.is_builtin_mod && !struct_sym.info.is_anon {
			c.check_valid_pascal_case(node.name, 'struct name', node.pos)
		}
		for embed in node.embeds {
			embed_sym := c.table.sym(embed.typ)
			if embed_sym.info is ast.Alias {
				parent_sym := c.table.sym(embed_sym.info.parent_type)
				if parent_sym.kind != .struct {
					c.error('`${embed_sym.name}` (alias of `${parent_sym.name}`) is not a struct',
						embed.pos)
				}
			} else if embed_sym.kind != .struct {
				c.error('`${embed_sym.name}` is not a struct', embed.pos)
			} else if (embed_sym.info as ast.Struct).is_heap && !embed.typ.is_ptr() {
				struct_sym.info.is_heap = true
			}
			embed_is_generic := embed.typ.has_flag(.generic)
			if embed_is_generic {
				has_generic_types = true
			}
			// Ensure each generic type of the embed was declared in the struct's definition
			if embed_is_generic && node.generic_types.len > 0 {
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
			aligned := if attr.arg == '' { 0 } else { attr.arg.int() }
			if aligned > 1 {
				c.table.used_features.memory_align = true
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
								field.pos)
						}
					}
				}
			}
			// Do not allow uninitialized `fn` fields, or force `?fn`
			// (allow them in `C.` structs)
			if !c.is_builtin_mod && node.language == .v {
				if !(c.file.is_translated || c.pref.translated) {
					sym := c.table.sym(field.typ)
					if sym.kind == .function && !field.typ.has_flag(.option)
						&& !field.has_default_expr && !field.attrs.contains('required') {
						error_msg := 'uninitialized `fn` struct fields are not allowed, since they can result in segfaults; use `?fn` or `@[required]` or initialize the field with `=` (if you absolutely want to have unsafe function pointers, use `= unsafe { nil }`)'
						c.note(error_msg, field.pos)
					}
				}
			}

			if field.has_default_expr {
				c.expected_type = field.typ
				field.default_expr_typ = c.expr(mut field.default_expr)
				if field.typ.is_ptr() != field.default_expr_typ.is_ptr() {
					match field.default_expr {
						ast.CallExpr {
							err_desc := if field.typ.is_ptr() { 'is' } else { 'is not' }
							val_desc := if field.default_expr_typ.is_ptr() { 'is' } else { 'is not' }
							c.error('field ${err_desc} reference but default value ${val_desc} reference',
								field.default_expr.pos())
						}
						ast.StructInit {
							c.error('reference field must be initialized with reference',
								field.default_expr.pos())
						}
						else {}
					}
				}

				if c.table.final_sym(field.typ).kind == .voidptr
					&& field.default_expr_typ !in [ast.nil_type, ast.voidptr_type, ast.byteptr_type]
					&& !field.default_expr_typ.is_ptr()
					&& (field.default_expr !is ast.IntegerLiteral
					|| (field.default_expr is ast.IntegerLiteral
					&& field.default_expr.val.int() != 0)) {
					c.note('voidptr variables may only be assigned voidptr values (e.g. unsafe { voidptr(${field.default_expr.str()}) })',
						field.default_expr.pos())
				}

				// disallow map `mut a = b`
				field_sym := c.table.sym(field.typ)
				expr_sym := c.table.sym(field.default_expr_typ)
				if field_sym.kind == .map && expr_sym.kind == .map && field.default_expr.is_lvalue()
					&& field.is_mut
					&& (!field.default_expr_typ.is_ptr() || field.default_expr is ast.Ident) {
					c.error('cannot copy map: call `clone` method (or use a reference)',
						field.default_expr.pos())
				}
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
			field_is_generic := field.typ.has_flag(.generic)
			if c.table.type_kind(field.typ) != .alias
				&& !c.ensure_generic_type_specify_type_names(field.typ, field.type_pos, c.table.final_sym(field.typ).kind in [.array, .array_fixed, .map], field_is_generic) {
				continue
			}
			if field_is_generic {
				has_generic_types = true
			}
			if node.language == .v {
				c.check_valid_snake_case(field.name, 'field name', field.pos)
			}
			sym := c.table.sym(field.typ)
			field_name, field_name_len := field.name, field.name.len
			for j in 0 .. i {
				if field_name_len == node.fields[j].name.len && field_name == node.fields[j].name {
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
				.struct {
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
				.none {
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
						c.error('byte is deprecated, use u8 instead', field.type_pos)
					}
				}
				else {
					c.check_any_type(field.typ, sym, field.type_pos)
				}
			}

			if field.has_default_expr {
				c.expected_type = field.typ
				if !field.typ.has_option_or_result() {
					c.check_expr_option_or_result_call(field.default_expr, field.default_expr_typ)
				}
				if sym.info is ast.ArrayFixed && field.typ == field.default_expr_typ {
					if sym.info.size_expr is ast.ComptimeCall {
						// field [$d('x' ,2)]int = [1 ,2]!
						if sym.info.size_expr.method_name == 'd' {
							c.error('cannot initialize a fixed size array field that uses `\$d()` as size quantifier since the size may change via -d',
								field.default_expr.pos())
						}
					}
				}
				if field.default_expr is ast.ArrayInit {
					if c.table.final_sym(field.default_expr_typ).kind in [.array, .array_fixed]
						&& c.table.value_type(field.default_expr_typ) == struct_typ_idx {
						c.error('cannot initialize array of same struct type that is being defined (recursion detected)',
							field.pos)
					}
				}
				interface_implemented := sym.kind == .interface
					&& c.type_implements(field.default_expr_typ, field.typ, field.pos)
				c.check_expected(field.default_expr_typ, field.typ) or {
					if sym.kind == .interface && interface_implemented {
						if !c.inside_unsafe && !field.default_expr_typ.is_any_kind_of_pointer() {
							if c.table.sym(field.default_expr_typ).kind != .interface {
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
					field_is_option := field.typ.has_flag(.option)
					if field_is_option {
						if field.default_expr is ast.None {
							c.warn('unnecessary default value of `none`: struct fields are zeroed by default',
								field.default_expr.pos)
						} else if field.default_expr.is_nil() {
							c.error('cannot assign `nil` to option value', field.default_expr.pos())
						}
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
	// Handle `implements` if it's present
	if node.is_implements {
		// XTODO2
		// cgen error if I use `println(sym)` without handling the option with `or{}`
		struct_type := c.table.find_type(node.name) // or { panic(err) }
		mut names_used := []string{}
		for t in node.implements_types {
			t_sym := c.table.sym(t.typ)
			if t_sym.info is ast.Interface {
				if t_sym.info.is_generic {
					itype_name := c.table.type_to_str(t.typ)
					if !itype_name.contains('[') {
						c.error('missing generic type on ${t_sym.name}', t.pos)
					}
					if itype_name.contains('<') {
						struct_generic_letters := node.generic_types.map(c.table.type_to_str(it))
						unknown_letters := itype_name.all_after('<').all_before('>').split(',').filter(it !in struct_generic_letters)
						if unknown_letters.len > 0 {
							c.error('unknown generic type ${unknown_letters.first()}',
								t.pos)
						}
					}
				}
				variant_name := c.table.type_to_str(t.typ)
				if variant_name in names_used {
					c.error('struct type ${node.name} cannot implement interface `${t_sym.name} more than once`',
						t.pos)
				}
				names_used << variant_name
			} else {
				c.error('`${t_sym.name}` is not an interface type', t.pos)
			}
			c.type_implements(struct_type, t.typ, node.pos)
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
			return if b_sym.info is ast.Enum {
				match true {
					a_sym.info.vals.len > b_sym.info.vals.len { -1 }
					a_sym.info.vals.len < b_sym.info.vals.len { 1 }
					else { 0 }
				}
			} else {
				1
			}
		}
	} else if b_sym.info is ast.Enum {
		if !b_sym.info.is_flag && !b_sym.info.uses_exprs {
			return -1
		}
	}

	a_size, a_align := t.type_size(a.typ)
	b_size, b_align := t.type_size(b.typ)
	return match true {
		a_align > b_align { -1 }
		a_align < b_align { 1 }
		a_size > b_size { -1 }
		a_size < b_size { 1 }
		else { 0 }
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
	if c.pref.skip_unused && !c.is_builtin_mod && !c.table.used_features.external_types {
		type_str := c.table.type_to_str(node.typ)
		c.table.used_features.external_types = type_str.contains('.') && type_str.len > 1
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
		if struct_sym.info.is_union && node.init_fields.len > 1 {
			c.error('union `${struct_sym.name}` can have only one field initialised',
				node.pos)
		}
	} else if struct_sym.info is ast.FnType {
		c.error('functions must be defined, not instantiated like structs', node.pos)
	}
	// register generic struct type when current fn is generic fn
	if c.table.cur_fn != unsafe { nil } && c.table.cur_fn.generic_names.len > 0 {
		c.table.unwrap_generic_type_ex(node.typ, c.table.cur_fn.generic_names, c.table.cur_concrete_types,
			true)
		if c.pref.skip_unused && node.typ.has_flag(.generic) {
			c.table.used_features.comptime_syms[c.unwrap_generic(node.typ)] = true
		}
	}
	if !is_field_zero_struct_init {
		c.ensure_type_exists(node.typ, node.pos)
	}
	type_sym := c.table.sym(node.typ)
	// Make sure the first letter is capital, do not allow e.g. `x := string{}`,
	// but `x := T{}` is ok.
	if !c.is_builtin_mod && !c.inside_unsafe && type_sym.language == .v
		&& c.table.cur_concrete_types.len == 0 {
		pos := type_sym.name.last_index_u8(`.`)
		first_letter := type_sym.name[pos + 1]
		if !first_letter.is_capital() && type_sym.kind != .none
			&& (type_sym.kind != .struct || !(type_sym.info is ast.Struct && type_sym.info.is_anon))
			&& type_sym.kind != .placeholder {
			c.error('cannot initialize builtin type `${type_sym.name}`', node.pos)
		}
		if type_sym.kind == .enum && !c.pref.translated && !c.file.is_translated {
			c.error('cannot initialize enums', node.pos)
		}
	}
	if type_sym.kind == .sum_type && node.init_fields.len == 1 {
		sexpr := node.init_fields[0].expr.str()
		c.error('cast to sum type using `${type_sym.name}(${sexpr})` not `${type_sym.name}{${sexpr}}`',
			node.pos)
	}
	if type_sym.kind == .interface && type_sym.language != .js {
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
	if type_sym.info is ast.Struct && type_sym.mod != c.mod {
		for attr in type_sym.info.attrs {
			match attr.name {
				'noinit' {
					c.error(
						'struct `${type_sym.name}` is declared with a `@[noinit]` attribute, so ' +
						'it cannot be initialized with `${type_sym.name}{}`', node.pos)
				}
				'deprecated' {
					c.deprecate('struct', type_sym.name, type_sym.info.attrs, node.pos)
				}
				else {}
			}
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
			if sym.info is ast.Struct {
				if sym.mod != c.mod {
					for attr in sym.info.attrs {
						match attr.name {
							'noinit' {
								c.error(
									'struct `${sym.name}` is declared with a `@[noinit]` attribute, so ' +
									'it cannot be initialized with `${sym.name}{}`', node.pos)
							}
							'deprecated' {
								c.deprecate('struct', sym.name, sym.info.attrs, node.pos)
							}
							else {}
						}
					}
				}
				if node.no_keys && node.init_fields.len != sym.info.fields.len {
					fname := if sym.info.fields.len != 1 { 'fields' } else { 'field' }
					c.error('initializing struct `${sym.name}` needs `${sym.info.fields.len}` ${fname}, but got `${node.init_fields.len}`',
						node.pos)
				}
			}
		}
		// string & array are also structs but .kind of string/array
		.struct, .string, .array, .alias {
			mut info := ast.Struct{}
			if type_sym.kind == .alias {
				info_t := type_sym.info as ast.Alias
				sym := c.table.sym(info_t.parent_type)
				if sym.kind == .placeholder { // pending import symbol did not resolve
					c.error('unknown struct: ${type_sym.name}', node.pos)
					return ast.void_type
				}
				match sym.kind {
					.struct {
						info = sym.info as ast.Struct
					}
					.array, .array_fixed, .map {
						// we do allow []int{}, [10]int{}, map[string]int{}
					}
					else {
						c.error('alias type name: ${sym.name} is not struct type', node.pos)
					}
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
				mut got_type := ast.no_type
				mut exp_type := ast.no_type
				inited_fields << field_name
				exp_type = field_info.typ
				exp_type_sym := c.table.sym(exp_type)
				c.expected_type = exp_type
				got_type = c.expr(mut init_field.expr)
				got_type_sym := c.table.sym(got_type)
				if got_type == ast.void_type {
					c.error('`${init_field.expr}` (no value) used as value', init_field.pos)
				}
				exp_type_is_option := exp_type.has_flag(.option)
				if !exp_type_is_option {
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
				if exp_type_is_option && got_type.is_ptr() && !exp_type.is_ptr() {
					c.error('cannot assign a pointer to option struct field', init_field.pos)
				}
				if exp_type_sym.kind == .voidptr {
					if got_type_sym.kind == .struct && !got_type.is_ptr() {
						c.error('allocate `${got_type_sym.name}` on the heap for use in other functions',
							init_field.pos)
					} else if got_type !in [ast.nil_type, ast.voidptr_type, ast.byteptr_type]
						&& !got_type.is_ptr() && (init_field.expr !is ast.IntegerLiteral
						|| (init_field.expr is ast.IntegerLiteral
						&& init_field.expr.val.int() != 0)) {
						c.note('voidptr variables may only be assigned voidptr values (e.g. unsafe { voidptr(${init_field.expr.str()}) })',
							init_field.expr.pos())
					}
				}
				// disallow `mut a: b`, when b is const map
				if exp_type_sym.kind == .map && got_type_sym.kind == .map && !got_type.is_ptr()
					&& field_info.is_mut
					&& (init_field.expr is ast.Ident && init_field.expr.obj is ast.ConstField) {
					c.error('cannot assign a const map to mut struct field, call `clone` method (or use a reference)',
						init_field.expr.pos())
				}
				if exp_type_sym.kind == .array && got_type_sym.kind == .array {
					if init_field.expr is ast.IndexExpr && init_field.expr.left is ast.Ident
						&& ((init_field.expr as ast.IndexExpr).left.is_mut()
						|| field_info.is_mut) && init_field.expr.index is ast.RangeExpr
						&& !c.inside_unsafe {
						// `a: arr[..]` auto add clone() -> `a: arr[..].clone()`
						c.add_error_detail_with_pos('To silence this notice, use either an explicit `a[..].clone()`,
or use an explicit `unsafe{ a[..] }`, if you do not want a copy of the slice.',
							init_field.expr.pos())
						c.note('an implicit clone of the slice was done here', init_field.expr.pos())
						mut right := ast.CallExpr{
							name:           'clone'
							left:           init_field.expr
							left_type:      got_type
							is_method:      true
							receiver_type:  got_type.ref()
							return_type:    got_type
							scope:          c.fn_scope
							is_return_used: true
						}
						got_type = c.expr(mut right)
						node.init_fields[i].expr = right
					}
					// disallow `mut a: b`, when b is const array
					if field_info.is_mut
						&& (init_field.expr is ast.Ident && init_field.expr.obj is ast.ConstField)
						&& !c.inside_unsafe {
						c.error('cannot assign a const array to mut struct field, call `clone` method (or use `unsafe`)',
							init_field.expr.pos())
					}
				}
				if exp_type_sym.kind == .interface {
					if c.type_implements(got_type, exp_type, init_field.pos) {
						if !c.inside_unsafe && got_type_sym.kind != .interface
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
					if !c.inside_unsafe && type_sym.language == .v && !(c.file.is_translated
						|| c.pref.translated) && exp_type.is_ptr()
						&& !got_type.is_any_kind_of_pointer() && !exp_type_is_option
						&& !(init_field.expr is ast.UnsafeExpr && init_field.expr.expr.str() == '0') {
						if init_field.expr.str() == '0' {
							c.error('assigning `0` to a reference field is only allowed in `unsafe` blocks',
								init_field.pos)
						} else {
							c.error('reference field must be initialized with reference',
								init_field.pos)
						}
					} else if exp_type.is_any_kind_of_pointer()
						&& !got_type.is_any_kind_of_pointer() && !got_type.is_int()
						&& (!exp_type_is_option || got_type.idx() != ast.none_type_idx) {
						got_typ_str := c.table.type_to_str(got_type)
						exp_typ_str := c.table.type_to_str(exp_type)
						c.error('cannot assign to field `${field_info.name}`: expected a pointer `${exp_typ_str}`, but got `${got_typ_str}`',
							init_field.pos)
					}
				}
				node.init_fields[i].typ = got_type
				node.init_fields[i].expected_type = exp_type

				if got_type.is_ptr() && exp_type.is_ptr() && mut init_field.expr is ast.Ident
					&& !info.is_heap {
					c.fail_if_stack_struct_action_outside_unsafe(mut init_field.expr,
						'assigned')
				}
				if c.table.unaliased_type(exp_type) in ast.unsigned_integer_type_idxs
					&& mut init_field.expr is ast.IntegerLiteral
					&& (init_field.expr as ast.IntegerLiteral).val[0] == `-` {
					c.error('cannot assign negative value to unsigned integer type', init_field.expr.pos)
				}

				if exp_type_sym.info is ast.Struct && !exp_type_sym.info.is_anon
					&& mut init_field.expr is ast.StructInit && init_field.expr.is_anon {
					c.error('cannot assign anonymous `struct` to a typed `struct`', init_field.expr.pos)
				}

				// all the fields of initialized embedded struct are ignored, they are considered initialized
				sym := c.table.sym(init_field.typ)
				if init_field.is_embed && sym.kind == .struct && sym.language == .v {
					struct_fields := c.table.struct_fields(sym)
					for struct_field in struct_fields {
						inited_fields << struct_field.name
					}
				}
				expected_type_sym := c.table.final_sym(init_field.expected_type)
				if expected_type_sym.kind in [.string, .array, .map, .array_fixed, .chan, .struct]
					&& init_field.expr.is_nil() && !init_field.expected_type.is_ptr()
					&& mut init_field.expr is ast.UnsafeExpr {
					c.error('cannot assign `nil` to struct field `${init_field.name}` with type `${expected_type_sym.name}`',
						init_field.expr.pos.extend(init_field.expr.expr.pos()))
				}
			}
			if !node.has_update_expr {
				c.check_uninitialized_struct_fields_and_embeds(node, type_sym, mut info, mut
					inited_fields)
			}
			// println('>> checked_types.len: $checked_types.len | checked_types: $checked_types | type_sym: $type_sym.name ')
		}
		.sum_type {
			first_typ := (type_sym.info as ast.SumType).variants[0]
			first_sym := c.table.final_sym(first_typ)
			if first_sym.kind == .struct {
				mut info := first_sym.info as ast.Struct
				c.check_uninitialized_struct_fields_and_embeds(node, first_sym, mut info, mut
					inited_fields)
			} else if first_sym.kind == .array {
				c.table.used_features.arr_init = true
			}
		}
		.none {
			// var := struct { name: "" }
			mut init_fields := []ast.StructField{}
			for mut init_field in node.init_fields {
				mut expr := unsafe { init_field }
				init_field.typ = c.expr(mut expr.expr)
				init_field.expected_type = init_field.typ
				init_fields << ast.StructField{
					name:   init_field.name
					typ:    init_field.typ
					is_mut: c.anon_struct_should_be_mut
				}
			}
			c.table.anon_struct_counter++
			name := '_VAnonStruct${c.table.anon_struct_counter}'
			sym_struct := ast.TypeSymbol{
				kind:     .struct
				language: .v
				name:     name
				cname:    util.no_dots(name)
				mod:      c.mod
				info:     ast.Struct{
					is_anon: true
					fields:  init_fields
				}
				is_pub:   true
			}
			ret := c.table.register_sym(sym_struct)
			c.table.register_anon_struct(name, ret)
			node.typ = c.table.find_type_idx(name)
		}
		else {}
	}
	if node.has_update_expr {
		update_type := c.expr(mut node.update_expr)
		node.update_expr_type = update_type
		expr_sym := c.table.final_sym(c.unwrap_generic(update_type))
		if node.update_expr is ast.ComptimeSelector {
			c.error('cannot use struct update syntax in compile time expressions', node.update_expr_pos)
		} else if expr_sym.kind != .struct {
			s := c.table.type_to_str(update_type)
			c.error('expected struct, found `${s}`', node.update_expr.pos())
		} else if update_type != node.typ {
			from_sym := c.table.final_sym(update_type)
			to_sym := c.table.final_sym(node.typ)
			from_info := from_sym.info as ast.Struct
			to_info := to_sym.info as ast.Struct
			// TODO: this check is too strict
			if !c.check_struct_signature(from_info, to_info)
				|| !c.check_struct_signature_init_fields(from_info, to_info, node) {
				c.error('struct `${from_sym.name}` is not compatible with struct `${to_sym.name}`',
					node.update_expr.pos())
			}
		}
	}
	if struct_sym.info is ast.Struct && struct_sym.info.generic_types.len > 0
		&& c.table.cur_concrete_types.len == 0 {
		if struct_sym.info.concrete_types.len == 0 {
			concrete_types := c.infer_struct_generic_types(node.typ, node)
			if concrete_types.len > 0 {
				generic_names := struct_sym.info.generic_types.map(c.table.sym(it).name)
				node.typ = c.table.unwrap_generic_type(node.typ, generic_names, concrete_types)
			}
		} else if struct_sym.info.generic_types.len == struct_sym.info.concrete_types.len {
			parent_type := struct_sym.info.parent_type
			parent_sym := c.table.sym(parent_type)
			if c.inside_generic_struct_init {
				mut st := unsafe { struct_sym.info }
				for mut field in st.fields {
					sym := c.table.sym(field.typ)
					if sym.info is ast.ArrayFixed && c.array_fixed_has_unresolved_size(sym.info) {
						mut size_expr := unsafe { sym.info.size_expr }
						field.typ = c.eval_array_fixed_sizes(mut size_expr, 0, sym.info.elem_type)
					}
				}
			}
			for method in parent_sym.methods {
				generic_names := struct_sym.info.generic_types.map(c.table.sym(it).name)
				for i, param in method.params {
					if i == 0 || !param.typ.has_flag(.generic) {
						continue
					}
					param_sym := c.table.sym(param.typ)
					if param_sym.kind in [.struct, .interface, .sum_type] {
						c.table.unwrap_generic_type(param.typ, generic_names, struct_sym.info.concrete_types)
					}
				}
			}
		}
	}
	return node.typ
}

// Check uninitialized refs/sum types
// The variable `fields` contains two parts, the first part is the same as info.fields,
// and the second part is all fields embedded in the structure
// If the return value data composition form in `c.table.struct_fields()` is modified,
// need to modify here accordingly.
fn (mut c Checker) check_uninitialized_struct_fields_and_embeds(node ast.StructInit, type_sym ast.TypeSymbol, mut info ast.Struct, mut inited_fields []string) {
	mut fields := c.table.struct_fields(type_sym)
	mut checked_types := []ast.Type{}

	for i, mut field in fields {
		if field.name in inited_fields {
			if c.mod != type_sym.mod {
				if !field.is_pub {
					parts := type_sym.name.split('.')
					for init_field in node.init_fields {
						if field.name == init_field.name {
							mod_type := if parts.len > 1 {
								parts#[-2..].join('.')
							} else {
								parts.last()
							}
							if !c.inside_unsafe {
								c.error('cannot access private field `${field.name}` on `${mod_type}`',
									init_field.pos)

								break
							}
						}
					}
				}
				if field.is_deprecated {
					for init_field in node.init_fields {
						if field.name == init_field.name {
							c.deprecate('field', field.name, field.attrs, init_field.pos)
							break
						}
					}
				}
			}
			continue
		}
		sym := c.table.sym(field.typ)
		if field.is_embed && sym.info is ast.Struct {
			// struct embeds
			continue
		}
		if field.has_default_expr {
			if i < info.fields.len && field.default_expr_typ == 0 {
				if mut field.default_expr is ast.StructInit {
					idx := c.table.find_type(field.default_expr.typ_str)
					if idx != 0 {
						info.fields[i].default_expr_typ = ast.new_type(int(idx))
					}
				} else if field.default_expr.is_nil() {
					if field.typ.is_any_kind_of_pointer() {
						info.fields[i].default_expr_typ = field.typ
					}
				} else if field.default_expr is ast.Ident && field.default_expr.info is ast.IdentFn {
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
		field_is_option := field.typ.has_flag(.option)
		if field.typ.is_ptr() && !field.typ.has_flag(.shared_f) && !field_is_option
			&& !node.has_update_expr && !c.pref.translated && !c.file.is_translated {
			c.error('reference field `${type_sym.name}.${field.name}` must be initialized',
				node.pos)
			continue
		}
		if !field_is_option {
			if sym.kind == .struct {
				c.check_ref_fields_initialized(sym, mut checked_types, '${type_sym.name}.${field.name}',
					node.pos)
			} else if sym.kind == .alias {
				parent_sym := c.table.sym((sym.info as ast.Alias).parent_type)
				if parent_sym.kind == .struct {
					c.check_ref_fields_initialized(parent_sym, mut checked_types, '${type_sym.name}.${field.name}',
						node.pos)
				}
			}
		}
		// Do not allow empty uninitialized interfaces
		if sym.kind == .interface && !node.has_update_expr && !field_is_option
			&& sym.language != .js && !field.attrs.contains('noinit') {
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
		// Check for `@[required]` struct attr
		if !node.no_keys && !node.has_update_expr && field.attrs.contains('required')
			&& node.init_fields.all(it.name != field.name) {
			c.error('field `${type_sym.name}.${field.name}` must be initialized', node.pos)
		}
		if !node.has_update_expr && !field.has_default_expr && !field.typ.is_ptr()
			&& !field_is_option {
			field_final_sym := c.table.final_sym(field.typ)
			if field_final_sym.kind == .struct {
				mut zero_struct_init := ast.StructInit{
					pos: node.pos
					typ: field.typ
				}
				if field.is_part_of_union {
					if field.name in inited_fields {
						// fields that are part of an union, should only be checked, when they are explicitly initialised
						c.struct_init(mut zero_struct_init, true, mut inited_fields)
					}
				} else {
					c.struct_init(mut zero_struct_init, true, mut inited_fields)
				}
			}
		}
	}

	for embed in info.embeds {
		embed_sym := c.table.sym(embed)
		if embed_sym.info is ast.Struct {
			if embed_sym.info.is_union {
				mut embed_union_fields := c.table.struct_fields(embed_sym)
				mut found := false
				for init_field in inited_fields {
					for union_field in embed_union_fields {
						if init_field == union_field.name && found {
							c.error('embed union `${embed_sym.name}` can have only one field initialised',
								node.pos)
						}
						if init_field == union_field.name {
							found = true
						}
					}
				}
			}
		}
		mut zero_struct_init := ast.StructInit{
			pos: node.pos
			typ: embed
		}
		c.struct_init(mut zero_struct_init, true, mut inited_fields)
	}
}

// Recursively check whether the struct type field is initialized
fn (mut c Checker) check_ref_fields_initialized(struct_sym &ast.TypeSymbol, mut checked_types []ast.Type,
	linked_name string, pos &token.Pos) {
	if (c.pref.translated || c.file.is_translated) || struct_sym.language == .c {
		return
	}
	for field in c.table.struct_fields(struct_sym) {
		if field.typ in checked_types {
			continue
		}
		if field.typ.is_ptr() && !field.typ.has_flag(.shared_f) && !field.typ.has_flag(.option)
			&& !field.has_default_expr {
			c.error('reference field `${linked_name}.${field.name}` must be initialized (part of struct `${struct_sym.name}`)',
				pos)
			continue
		}
		sym := c.table.sym(field.typ)
		if sym.info is ast.Struct {
			if sym.language == .c {
				continue
			}
			if field.is_embed && sym.language == .v {
				// an embedded struct field
				continue
			}
			checked_types << field.typ
			c.check_ref_fields_initialized(sym, mut checked_types, '${linked_name}.${field.name}',
				pos)
		} else if sym.info is ast.Alias {
			psym := c.table.sym(sym.info.parent_type)
			if psym.kind == .struct {
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
fn (mut c Checker) check_ref_fields_initialized_note(struct_sym &ast.TypeSymbol, mut checked_types []ast.Type,
	linked_name string, pos &token.Pos) {
	if (c.pref.translated || c.file.is_translated) || struct_sym.language == .c {
		return
	}
	for field in c.table.struct_fields(struct_sym) {
		if field.typ in checked_types {
			continue
		}
		if field.typ.is_ptr() && !field.typ.has_flag(.shared_f) && !field.typ.has_flag(.option)
			&& !field.has_default_expr {
			c.note('reference field `${linked_name}.${field.name}` must be initialized (part of struct `${struct_sym.name}`)',
				pos)
			continue
		}
		sym := c.table.sym(field.typ)
		if sym.info is ast.Struct {
			if sym.language == .c {
				continue
			}
			if field.is_embed && sym.language == .v {
				// an embedded struct field
				continue
			}
			checked_types << field.typ
			c.check_ref_fields_initialized(sym, mut checked_types, '${linked_name}.${field.name}',
				pos)
		} else if sym.info is ast.Alias {
			psym := c.table.sym(sym.info.parent_type)
			if psym.kind == .struct {
				checked_types << field.typ
				c.check_ref_fields_initialized(psym, mut checked_types, '${linked_name}.${field.name}',
					pos)
			}
		}
	}
}

fn (mut c Checker) is_anon_struct_compatible(s1 ast.Struct, s2 ast.Struct) bool {
	if !(s1.is_anon && s2.is_anon && s1.fields.len == s2.fields.len) {
		return false
	}
	mut is_compatible := true
	for k, field in s1.fields {
		if !c.check_basic(field.typ, s2.fields[k].typ) {
			is_compatible = false
			break
		}
	}
	return is_compatible
}
