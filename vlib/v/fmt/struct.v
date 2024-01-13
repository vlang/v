// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fmt

import strings
import v.ast

pub fn (mut f Fmt) struct_decl(node ast.StructDecl, is_anon bool) {
	f.attrs(node.attrs)
	if node.is_pub && !is_anon {
		f.write('pub ')
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
	mut field_aligns := []AlignInfo{}
	mut comment_aligns := []AlignInfo{}
	mut default_expr_aligns := []AlignInfo{}
	mut field_types := []string{cap: node.fields.len}
	// Calculate the alignments first
	f.calculate_alignment(node.fields, mut field_aligns, mut comment_aligns, mut default_expr_aligns, mut
		field_types)
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
	mut field_align_i := 0
	mut comment_align_i := 0
	mut default_expr_align_i := 0
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
			i > 0 {
				// keep one empty line between fields (exclude one after mut:, pub:, ...)
				last_field := node.fields[i - 1]
				before_last_line := if last_field.comments.len > 0
					&& last_field.pos.line_nr < last_field.comments.last().pos.last_line {
					last_field.comments.last().pos.last_line
				} else if last_field.has_default_expr {
					last_field.default_expr.pos().last_line
				} else {
					last_field.pos.line_nr
				}

				next_first_line := if field.comments.len > 0
					&& field.pos.line_nr > field.comments[0].pos.line_nr {
					field.comments[0].pos.line_nr
				} else {
					field.pos.line_nr
				}

				if next_first_line - before_last_line > 1 {
					f.writeln('')
				}
			}
			else {}
		}
		end_pos := field.pos.pos + field.pos.len
		before_comments := field.comments.filter(it.pos.pos < field.pos.pos)
		between_comments := field.comments[before_comments.len..].filter(it.pos.pos < end_pos)
		after_type_comments := field.comments[(before_comments.len + between_comments.len)..]
		// Handle comments before the field
		f.comments_before_field(before_comments)
		volatile_prefix := if field.is_volatile { 'volatile ' } else { '' }
		f.write('\t${volatile_prefix}${field.name} ')
		// Handle comments between field name and type
		before_len := f.line_len
		f.comments(between_comments, has_nl: false)
		comments_len := f.line_len - before_len
		if field_aligns[field_align_i].line_nr < field.pos.line_nr {
			field_align_i++
		}
		field_align := field_aligns[field_align_i]
		f.write(strings.repeat(` `, field_align.max_len - field.name.len - comments_len))
		// Handle anon structs recursively
		if !f.write_anon_struct_field_decl(field.typ, field.anon_struct_decl) {
			f.write(field_types[i])
		}
		f.mark_types_import_as_used(field.typ)
		attrs_len := inline_attrs_len(field.attrs)
		has_attrs := field.attrs.len > 0
		// has_at := if has_attrs { field.attrs[0].has_at } else { false }
		has_at := true
		// TODO: this will get removed in next stage
		if has_attrs && !has_at {
			f.write(strings.repeat(` `, field_align.max_type_len - field_types[i].len))
			f.single_line_attrs(field.attrs, same_line: true)
		}
		if field.has_default_expr {
			if default_expr_aligns[default_expr_align_i].line_nr < field.pos.line_nr {
				default_expr_align_i++
			}
			align := default_expr_aligns[default_expr_align_i]
			pad_len := align.max_len - attrs_len + align.max_type_len - field_types[i].len
			f.write(strings.repeat(` `, pad_len))
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
		if has_attrs && has_at {
			f.write(strings.repeat(` `, field_align.max_type_len - field_types[i].len))
			f.single_line_attrs(field.attrs, same_line: true)
		}
		// Handle comments after field type
		if after_type_comments.len > 0 {
			if after_type_comments[0].pos.line_nr > field.pos.line_nr {
				f.writeln('')
			} else {
				if !field.has_default_expr {
					if comment_aligns[comment_align_i].line_nr < field.pos.line_nr {
						comment_align_i++
					}
					align := comment_aligns[comment_align_i]
					pad_len := align.max_len - attrs_len + align.max_type_len - field_types[i].len
					f.write(strings.repeat(` `, pad_len))
				}
				f.write(' ')
			}
			f.comments(after_type_comments, level: .indent)
		} else {
			f.writeln('')
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
		.struct_ {
			info := sym.info as ast.Struct
			if info.is_anon {
				f.indent++
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
		f.no_cur_mod(f.short_module(sym_name)) // TODO f.type_to_str?
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
			for i, init_field in node.init_fields {
				f.write('${init_field.name}: ')
				f.expr(init_field.expr)
				f.comments(init_field.comments, same_line: true, has_nl: false, level: .indent)
				if single_line_fields {
					if i < node.init_fields.len - 1 {
						f.write(', ')
					}
				} else {
					f.writeln('')
				}
				f.comments(init_field.next_comments, has_nl: true, level: .keep)
				if single_line_fields && (init_field.comments.len > 0
					|| init_field.next_comments.len > 0
					|| !expr_is_single_line(init_field.expr)
					|| f.line_len > max_len.last()) {
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
