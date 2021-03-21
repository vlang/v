// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import v.table
import v.pref

pub fn (mut c Checker) struct_decl(mut decl ast.StructDecl) {
	if decl.language == .v && !c.is_builtin_mod {
		c.check_valid_pascal_case(decl.name, 'struct name', decl.pos)
	}
	mut struct_sym := c.table.find_type(decl.name) or { table.TypeSymbol{} }
	if mut struct_sym.info is table.Struct {
		for embed in decl.embeds {
			embed_sym := c.table.get_type_symbol(embed.typ)
			if embed_sym.kind != .struct_ {
				c.error('`$embed_sym.name` is not a struct', embed.pos)
			} else {
				info := embed_sym.info as table.Struct
				if info.is_heap && !embed.typ.is_ptr() {
					struct_sym.info.is_heap = true
				}
			}
		}
		for attr in decl.attrs {
			if attr.name == 'typedef' && decl.language != .c {
				c.error('`typedef` attribute can only be used with C structs', decl.pos)
			}
		}
		for i, field in decl.fields {
			c.ensure_type_exists(field.typ, field.type_pos) or { return }
			if decl.language == .v {
				c.check_valid_snake_case(field.name, 'field name', field.pos)
			}
			sym := c.table.get_type_symbol(field.typ)
			for j in 0 .. i {
				if field.name == decl.fields[j].name {
					c.error('field name `$field.name` duplicate', field.pos)
				}
			}
			if sym.kind == .struct_ {
				info := sym.info as table.Struct
				if info.is_heap && !field.typ.is_ptr() {
					struct_sym.info.is_heap = true
				}
			}
			if field.has_default_expr {
				c.expected_type = field.typ
				field_expr_type := c.expr(field.default_expr)
				struct_sym.info.fields[i].default_expr_typ = field_expr_type
				c.check_expected(field_expr_type, field.typ) or {
					if !(sym.kind == .interface_
						&& c.type_implements(field_expr_type, field.typ, field.pos)) {
						c.error('incompatible initializer for field `$field.name`: $err.msg',
							field.default_expr.position())
					}
				}
				// Check for unnecessary inits like ` = 0` and ` = ''`
				if field.typ.is_ptr() {
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
	}
}

pub fn (mut c Checker) struct_init(mut struct_init ast.StructInit) table.Type {
	// typ := c.table.find_type(struct_init.typ.typ.name) or {
	// c.error('unknown struct: $struct_init.typ.typ.name', struct_init.pos)
	// panic('')
	// }
	if struct_init.typ == table.void_type {
		// Short syntax `({foo: bar})`
		if c.expected_type == table.void_type {
			c.error('unexpected short struct syntax', struct_init.pos)
			return table.void_type
		}
		sym := c.table.get_type_symbol(c.expected_type)
		if sym.kind == .array {
			struct_init.typ = c.table.value_type(c.expected_type)
		} else {
			struct_init.typ = c.expected_type
		}
	}
	utyp := c.unwrap_generic(struct_init.typ)
	c.ensure_type_exists(utyp, struct_init.pos) or {}
	type_sym := c.table.get_type_symbol(utyp)
	// Make sure the first letter is capital, do not allow e.g. `x := string{}`,
	// but `x := T{}` is ok.
	if !c.is_builtin_mod && !c.inside_unsafe && type_sym.language == .v
		&& c.cur_generic_types.len == 0 {
		pos := type_sym.name.last_index('.') or { -1 }
		first_letter := type_sym.name[pos + 1]
		if !first_letter.is_capital() {
			c.error('cannot initialize builtin type `$type_sym.name`', struct_init.pos)
		}
	}
	if type_sym.kind == .sum_type && struct_init.fields.len == 1 {
		sexpr := struct_init.fields[0].expr.str()
		c.error('cast to sum type using `${type_sym.name}($sexpr)` not `$type_sym.name{$sexpr}`',
			struct_init.pos)
	}
	if type_sym.kind == .interface_ {
		c.error('cannot instantiate interface `$type_sym.name`', struct_init.pos)
	}
	if type_sym.info is table.Alias {
		if type_sym.info.parent_type.is_number() {
			c.error('cannot instantiate number type alias `$type_sym.name`', struct_init.pos)
			return table.void_type
		}
	}
	// allow init structs from generic if they're private except the type is from builtin module
	if !type_sym.is_public && type_sym.kind != .placeholder && type_sym.language != .c
		&& (type_sym.mod != c.mod && !(struct_init.typ.has_flag(.generic)
		&& type_sym.mod != 'builtin')) {
		c.error('type `$type_sym.name` is private', struct_init.pos)
	}
	if type_sym.kind == .struct_ {
		info := type_sym.info as table.Struct
		if info.attrs.len > 0 && info.attrs[0].name == 'noinit' && type_sym.mod != c.mod {
			c.error('struct `$type_sym.name` is declared with a `[noinit]` attribute, so ' +
				'it cannot be initialized with `$type_sym.name{}`', struct_init.pos)
		}
		if info.is_heap && !c.inside_ref_lit && !c.inside_unsafe && !struct_init.typ.is_ptr() {
			c.error('`$type_sym.name` type can only be used as a reference `&$type_sym.name` or inside a `struct` reference',
				struct_init.pos)
		}
	}
	if type_sym.name.len == 1 && c.cur_fn.generic_params.len == 0 {
		c.error('unknown struct `$type_sym.name`', struct_init.pos)
		return 0
	}
	match type_sym.kind {
		.placeholder {
			c.error('unknown struct: $type_sym.name', struct_init.pos)
			return table.void_type
		}
		// string & array are also structs but .kind of string/array
		.struct_, .string, .array, .alias {
			mut info := table.Struct{}
			if type_sym.kind == .alias {
				info_t := type_sym.info as table.Alias
				sym := c.table.get_type_symbol(info_t.parent_type)
				if sym.kind == .placeholder { // pending import symbol did not resolve
					c.error('unknown struct: $type_sym.name', struct_init.pos)
					return table.void_type
				}
				if sym.kind != .struct_ {
					c.error('alias type name: $sym.name is not struct type', struct_init.pos)
				}
				info = sym.info as table.Struct
			} else {
				info = type_sym.info as table.Struct
			}
			if struct_init.is_short {
				exp_len := info.fields.len
				got_len := struct_init.fields.len
				if exp_len != got_len {
					amount := if exp_len < got_len { 'many' } else { 'few' }
					c.error('too $amount fields in `$type_sym.name` literal (expecting $exp_len, got $got_len)',
						struct_init.pos)
				}
			}
			mut inited_fields := []string{}
			for i, field in struct_init.fields {
				mut info_field := table.Field{}
				mut embed_type := table.Type(0)
				mut is_embed := false
				mut field_name := ''
				if struct_init.is_short {
					if i >= info.fields.len {
						// It doesn't make sense to check for fields that don't exist.
						// We should just stop here.
						break
					}
					info_field = info.fields[i]
					field_name = info_field.name
					struct_init.fields[i].name = field_name
				} else {
					field_name = field.name
					mut exists := false
					for f in info.fields {
						if f.name == field_name {
							info_field = f
							exists = true
							break
						}
					}
					if !exists {
						for embed in info.embeds {
							embed_sym := c.table.get_type_symbol(embed)
							if embed_sym.embed_name() == field_name {
								exists = true
								embed_type = embed
								is_embed = true
								break
							}
						}
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
				if is_embed {
					c.expected_type = embed_type
					expr_type := c.expr(field.expr)
					expr_type_sym := c.table.get_type_symbol(expr_type)
					if expr_type != table.void_type && expr_type_sym.kind != .placeholder {
						c.check_expected(expr_type, embed_type) or {
							c.error('cannot assign to field `$info_field.name`: $err.msg',
								field.pos)
						}
					}
					struct_init.fields[i].typ = expr_type
					struct_init.fields[i].expected_type = embed_type
				} else {
					inited_fields << field_name
					field_type_sym := c.table.get_type_symbol(info_field.typ)
					c.expected_type = info_field.typ
					expr_type := c.expr(field.expr)
					expr_type_sym := c.table.get_type_symbol(expr_type)
					if field_type_sym.kind == .interface_ {
						c.type_implements(expr_type, info_field.typ, field.pos)
					} else if expr_type != table.void_type && expr_type_sym.kind != .placeholder {
						c.check_expected(expr_type, info_field.typ) or {
							c.error('cannot assign to field `$info_field.name`: $err.msg',
								field.pos)
						}
					}
					if info_field.typ.has_flag(.shared_f) {
						if !expr_type.has_flag(.shared_f) && expr_type.is_ptr() {
							c.error('`shared` field must be initialized with `shared` or value',
								field.pos)
						}
					} else {
						if info_field.typ.is_ptr() && !expr_type.is_ptr() && !expr_type.is_pointer()
							&& !expr_type.is_number() {
							c.error('reference field must be initialized with reference',
								field.pos)
						}
					}
					struct_init.fields[i].typ = expr_type
					struct_init.fields[i].expected_type = info_field.typ
				}
			}
			// Check uninitialized refs
			for field in info.fields {
				if field.has_default_expr || field.name in inited_fields {
					continue
				}
				if field.typ.is_ptr() && !struct_init.has_update_expr && !c.pref.translated {
					c.error('reference field `${type_sym.name}.$field.name` must be initialized',
						struct_init.pos)
				}
				// Check for `[required]` struct attr
				if field.attrs.contains('required') && !struct_init.is_short {
					mut found := false
					for init_field in struct_init.fields {
						if field.name == init_field.name {
							found = true
							break
						}
					}
					if !found {
						c.error('field `${type_sym.name}.$field.name` must be initialized',
							struct_init.pos)
					}
				}
			}
		}
		else {}
	}
	if struct_init.has_update_expr {
		update_type := c.expr(struct_init.update_expr)
		struct_init.update_expr_type = update_type
		if c.table.type_kind(update_type) != .struct_ {
			s := c.table.type_to_str(update_type)
			c.error('expected struct, found `$s`', struct_init.update_expr.position())
		} else if update_type != struct_init.typ {
			from_sym := c.table.get_type_symbol(update_type)
			to_sym := c.table.get_type_symbol(struct_init.typ)
			from_info := from_sym.info as table.Struct
			to_info := to_sym.info as table.Struct
			// TODO this check is too strict
			if !c.check_struct_signature(from_info, to_info) {
				c.error('struct `$from_sym.name` is not compatible with struct `$to_sym.name`',
					struct_init.update_expr.position())
			}
		}
		if !struct_init.update_expr.is_lvalue() {
			// cgen will repeat `update_expr` for each field
			// so enforce an lvalue for efficiency
			c.error('expression is not an lvalue', struct_init.update_expr.position())
		}
	}
	return struct_init.typ
}

// check `to` has all fields of `from`
fn (c &Checker) check_struct_signature(from table.Struct, to table.Struct) bool {
	// Note: `to` can have extra fields
	if from.fields.len == 0 {
		return false
	}
	for field in from.fields {
		filtered := to.fields.filter(it.name == field.name)
		if filtered.len != 1 {
			// field doesn't exist
			return false
		}
		counterpart := filtered[0]
		if field.typ != counterpart.typ {
			// field has different tye
			return false
		}
		if field.is_pub != counterpart.is_pub {
			// field is not public while the other one is
			return false
		}
		if field.is_mut != counterpart.is_mut {
			// field is not mutable while the other one is
			return false
		}
	}
	return true
}
