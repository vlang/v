// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fmt

import v.ast

pub fn (mut f Fmt) struct_decl(node ast.StructDecl, is_anon bool) {
	f.attrs(node.attrs)
	if node.is_pub && !is_anon {
		f.write('pub ')
	}
	if node.is_option {
		f.write('?')
	}
	if node.is_union {
		f.write('union')
	} else {
		f.write('struct')
	}
	name := node.name.after('.') // strip prepended module
	if !is_anon {
		f.write(' ')
		f.write_language_prefix(node.language)
		f.write(name)
	}
	f.write_generic_types(node.generic_types)
	if node.fields.len == 0 && node.embeds.len == 0 && node.pos.line_nr == node.pos.last_line {
		f.writeln(' {}')
		return
	}
	mut type_align := new_field_align(use_break_line: true)
	mut default_expr_align := new_field_align(use_threshold: true)
	mut attr_align := new_field_align(use_threshold: true)
	mut comment_align := new_field_align(use_threshold: true)
	mut field_types := []string{cap: node.fields.len}
	if node.is_implements {
		f.write(' implements ')
		for i, t in node.implements_types {
			f.write(f.table.type_to_str_using_aliases(t.typ, f.mod2alias))
			if i < node.implements_types.len - 1 {
				f.write(', ')
			}
			f.mark_types_import_as_used(t.typ)
		}
	}
	// Calculate the alignments first
	f.calculate_alignment(node.fields, mut type_align, mut comment_align, mut default_expr_align, mut
		attr_align, mut field_types)
	f.writeln(' {')
	if node.pre_comments.len > 0 {
		f.comments_before_field(node.pre_comments)
	}
	for embed in node.embeds {
		f.mark_types_import_as_used(embed.typ)
		styp := f.table.type_to_str_using_aliases(embed.typ, f.mod2alias)

		pre_comments := embed.comments.filter(it.pos.pos < embed.pos.pos)
		comments := embed.comments[pre_comments.len..]

		f.comments_before_field(pre_comments)
		if comments.len == 0 {
			f.writeln('\t${styp}')
		} else {
			f.write('\t${styp}')
			f.comments(comments, level: .indent)
		}
	}
	// Now handle each field
	mut inc_indent := false // for correct indents with multi line default exprs
	for i, field in node.fields {
		match true {
			i == node.mut_pos {
				f.writeln('mut:')
			}
			i == node.pub_pos {
				f.writeln('pub:')
			}
			i == node.pub_mut_pos {
				f.writeln('pub mut:')
			}
			i == node.global_pos {
				f.writeln('__global:')
			}
			i == node.module_pos {
				f.writeln('module:')
			}
			i > 0 && field.has_prev_newline {
				f.writeln('')
			}
			else {}
		}
		// Handle comments before the field
		if field.pre_comments.len > 0 {
			f.comments(field.pre_comments, level: .indent)
		}
		volatile_prefix := if field.is_volatile { 'volatile ' } else { '' }
		f.write('\t${volatile_prefix}${field.name} ')
		f.write(' '.repeat(type_align.max_len(field.pos.line_nr) - field.name.len))
		// Handle anon structs recursively
		if !f.write_anon_struct_field_decl(field.typ, field.anon_struct_decl) {
			f.write(field_types[i])
		}
		f.mark_types_import_as_used(field.typ)
		attrs_len := inline_attrs_len(field.attrs)
		if field.has_default_expr {
			f.write(' '.repeat(default_expr_align.max_len(field.pos.line_nr) - field_types[i].len))
			f.write(' = ')
			if !expr_is_single_line(field.default_expr) {
				f.indent++
				inc_indent = true
			}
			f.expr(field.default_expr)
			if inc_indent {
				f.indent--
				inc_indent = false
			}
		}
		if field.attrs.len > 0 {
			f.write(' '.repeat(attr_align.max_len(field.pos.line_nr) - field_types[i].len))
			f.single_line_attrs(field.attrs, same_line: true)
		}
		// Handle comments at the end of the line
		if field.comments.len > 0 {
			if field.has_default_expr {
				f.write(' '.repeat(comment_align.max_len(field.pos.line_nr) - field.default_expr.str().len - 2))
			} else if field.attrs.len > 0 {
				f.write(' '.repeat(comment_align.max_len(field.pos.line_nr) - attrs_len))
			} else {
				f.write(' '.repeat(comment_align.max_len(field.pos.line_nr) - field_types[i].len))
			}
			f.write(' ')
			f.comments(field.comments, level: .indent)
		} else {
			f.writeln('')
		}
		// Handle comments on the next lines
		if field.next_comments.len > 0 {
			f.comments(field.next_comments, level: .indent)
		}
	}
	if is_anon || node.end_comments.len > 0 {
		f.write('}')
	} else {
		f.writeln('}')
	}
	if node.end_comments.len > 0 {
		f.comments(node.end_comments, same_line: true)
	}
}

fn (mut f Fmt) write_anon_struct_field_decl(field_typ ast.Type, field_anon_decl ast.StructDecl) bool {
	sym := f.table.sym(field_typ)
	match sym.kind {
		.struct {
			info := sym.info as ast.Struct
			if info.is_anon {
				f.indent++
				if info.is_shared {
					f.write('shared ')
				}
				f.struct_decl(field_anon_decl, true)
				f.indent--
				return true
			}
		}
		.array {
			if sym.info is ast.Array {
				elem_sym := f.table.sym(sym.info.elem_type)
				if elem_sym.info is ast.Struct {
					if elem_sym.info.is_anon {
						if field_typ.has_flag(.option) {
							f.write('?')
						}
						f.write('[]'.repeat(sym.info.nr_dims))
						f.write_anon_struct_field_decl(sym.info.elem_type, field_anon_decl)
						return true
					}
				}
			}
		}
		.array_fixed {
			if sym.info is ast.ArrayFixed {
				elem_sym := f.table.sym(sym.info.elem_type)
				if elem_sym.info is ast.Struct {
					if elem_sym.info.is_anon {
						f.write('[${sym.info.size}]')
						f.write_anon_struct_field_decl(sym.info.elem_type, field_anon_decl)
						return true
					}
				}
			}
		}
		else {}
	}
	return false
}

pub fn (mut f Fmt) struct_init(node ast.StructInit) {
	struct_init_save := f.is_struct_init
	f.is_struct_init = true
	defer {
		f.is_struct_init = struct_init_save
	}
	f.mark_types_import_as_used(node.typ)
	sym_name := f.table.sym(node.typ).name
	// f.write('<old name: $type_sym.name>')
	mut name := if !sym_name.starts_with('C.') && !sym_name.starts_with('JS.') {
		f.no_cur_mod(f.short_module(sym_name)) // TODO: f.type_to_str?
	} else {
		sym_name
	}
	if name == 'void' {
		name = ''
	}
	if node.typ.has_flag(.option) {
		f.write('?')
	}
	if node.is_anon {
		f.write('struct ')
	}
	if node.init_fields.len == 0 && !node.has_update_expr {
		// `Foo{}` on one line if there are no fields or comments
		if node.pre_comments.len == 0 {
			f.write('${name}{}')
		} else {
			f.writeln('${name}{')
			f.comments(node.pre_comments, same_line: true, has_nl: true, level: .indent)
			f.write('}')
		}
		f.mark_import_as_used(name)
	} else if node.no_keys {
		// `Foo{1,2,3}` (short syntax, no keys)
		f.write('${name}{')
		f.mark_import_as_used(name)
		if node.has_update_expr {
			f.write('...')
			f.expr(node.update_expr)
			f.write(', ')
		}
		for i, init_field in node.init_fields {
			f.expr(init_field.expr)
			if i < node.init_fields.len - 1 {
				f.write(', ')
			}
		}
		f.write('}')
	} else {
		use_short_args := f.use_short_fn_args && !node.has_update_expr
		f.use_short_fn_args = false
		mut single_line_fields := f.single_line_fields
		f.single_line_fields = false
		if node.pos.line_nr < node.pos.last_line || node.pre_comments.len > 0 {
			single_line_fields = false
		}
		if !use_short_args || node.is_anon {
			f.write('${name}{')
			f.mark_import_as_used(name)
			if single_line_fields {
				f.write(' ')
			}
		}
		fields_start := f.out.len
		fields_loop: for {
			if !single_line_fields {
				if use_short_args && f.out.last() == ` ` {
					//           v Remove space at tail of line
					// f(a, b, c, \n
					//     f1: 0\n
					//     f2: 1\n
					// )
					f.out.go_back(1)
				}
				f.writeln('')
				f.indent++
			}
			f.comments(node.pre_comments, same_line: true, has_nl: true, level: .keep)
			if node.has_update_expr {
				f.write('...')
				f.expr(node.update_expr)
				if single_line_fields {
					if node.init_fields.len > 0 {
						f.write(', ')
					}
				} else {
					f.writeln('')
				}
				f.comments(node.update_expr_comments, same_line: true, has_nl: true, level: .keep)
			}
			mut value_align := new_field_align(use_break_line: true)
			mut comment_align := new_field_align(use_threshold: true)
			for init_field in node.init_fields {
				value_align.add_info(init_field.name.len, init_field.pos.line_nr, init_field.has_break_line)
				if init_field.end_comments.len > 0 {
					comment_align.add_info(init_field.expr.str().len, init_field.pos.line_nr,
						init_field.has_break_line)
				}
			}
			for i, init_field in node.init_fields {
				if i > 0 && init_field.has_prev_newline {
					f.writeln('')
				}
				if init_field.pre_comments.len > 0 {
					f.comments(init_field.pre_comments, has_nl: true, level: .keep)
				}
				f.write('${init_field.name}: ')
				if !single_line_fields {
					f.write(' '.repeat(value_align.max_len(init_field.pos.line_nr) - init_field.name.len))
				}
				f.expr(init_field.expr)
				if init_field.end_comments.len > 0 {
					f.write(' '.repeat(
						comment_align.max_len(init_field.pos.line_nr) - init_field.expr.str().len +
						1))
					f.comments(init_field.end_comments, has_nl: false, level: .indent)
				}
				if single_line_fields {
					if i < node.init_fields.len - 1 {
						f.write(', ')
					}
				} else {
					f.writeln('')
				}
				f.comments(init_field.next_comments, has_nl: true, level: .keep)
				if single_line_fields && (init_field.end_comments.len > 0
					|| init_field.next_comments.len > 0
					|| !expr_is_single_line(init_field.expr) || f.line_len > max_len) {
					single_line_fields = false
					f.out.go_back_to(fields_start)
					f.line_len = fields_start
					f.remove_new_line()
					continue fields_loop
				}
			}
			break
		}
		if !single_line_fields {
			f.indent--
		}
		if !use_short_args || node.is_anon {
			if single_line_fields {
				f.write(' ')
			}
			f.write('}')
		}
	}
}
