// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fmt

import strings
import v.ast

pub fn (mut f Fmt) struct_decl(node ast.StructDecl) {
	f.attrs(node.attrs)
	if node.is_pub {
		f.write('pub ')
	}
	if node.is_union {
		f.write('union ')
	} else {
		f.write('struct ')
	}
	f.write_language_prefix(node.language)
	name := node.name.after('.') // strip prepended module
	f.write(name)
	f.write_generic_types(node.generic_types)
	if node.fields.len == 0 && node.embeds.len == 0 && node.pos.line_nr == node.pos.last_line {
		f.writeln(' {}')
		return
	}
	mut field_aligns := []AlignInfo{}
	mut comment_aligns := []AlignInfo{}
	mut default_expr_aligns := []AlignInfo{}
	mut field_types := []string{cap: node.fields.len}
	for i, field in node.fields {
		ft := f.no_cur_mod(f.table.type_to_str_using_aliases(field.typ, f.mod2alias))
		field_types << ft
		attrs_len := inline_attrs_len(field.attrs)
		end_pos := field.pos.pos + field.pos.len
		mut comments_len := 0 // Length of comments between field name and type
		for comment in field.comments {
			if comment.pos.pos >= end_pos {
				if comment.pos.line_nr == field.pos.line_nr {
					comment_aligns.add_info(attrs_len, field_types[i].len, comment.pos.line_nr,
						use_threshold: true)
				}
				continue
			}
			if comment.pos.pos > field.pos.pos {
				comments_len += '/* ${comment.text.trim_left('\x01')} */ '.len
			}
		}
		field_aligns.add_info(comments_len + field.name.len, ft.len, field.pos.line_nr)
		if field.has_default_expr {
			default_expr_aligns.add_info(attrs_len, field_types[i].len, field.pos.line_nr,
				use_threshold: true)
		}
	}
	f.writeln(' {')
	for embed in node.embeds {
		f.mark_types_import_as_used(embed.typ)
		styp := f.table.type_to_str_using_aliases(embed.typ, f.mod2alias)

		pre_comments := embed.comments.filter(it.pos.pos < embed.pos.pos)
		comments := embed.comments[pre_comments.len..]

		f.comments_before_field(pre_comments)
		if comments.len == 0 {
			f.writeln('\t$styp')
		} else {
			f.write('\t$styp')
			f.comments(comments, level: .indent)
		}
	}
	mut field_align_i := 0
	mut comment_align_i := 0
	mut default_expr_align_i := 0
	mut inc_indent := false // for correct indents with multi line default exprs
	for i, field in node.fields {
		if i == node.mut_pos {
			f.writeln('mut:')
		} else if i == node.pub_pos {
			f.writeln('pub:')
		} else if i == node.pub_mut_pos {
			f.writeln('pub mut:')
		} else if i == node.global_pos {
			f.writeln('__global:')
		} else if i == node.module_pos {
			f.writeln('module:')
		} else if i > 0 {
			// keep one empty line between fields (exclude one after mut:, pub:, ...)
			mut before_last_line := node.fields[i - 1].pos.line_nr
			if node.fields[i - 1].comments.len > 0 {
				if before_last_line < node.fields[i - 1].comments.last().pos.last_line {
					before_last_line = node.fields[i - 1].comments.last().pos.last_line
				}
			}
			if node.fields[i - 1].has_default_expr {
				if before_last_line < node.fields[i - 1].default_expr.pos().last_line {
					before_last_line = node.fields[i - 1].default_expr.pos().last_line
				}
			}

			mut next_first_line := field.pos.line_nr
			if field.comments.len > 0 {
				if next_first_line > field.comments[0].pos.line_nr {
					next_first_line = field.comments[0].pos.line_nr
				}
			}
			if next_first_line - before_last_line > 1 {
				f.writeln('')
			}
		}
		end_pos := field.pos.pos + field.pos.len
		before_comments := field.comments.filter(it.pos.pos < field.pos.pos)
		between_comments := field.comments[before_comments.len..].filter(it.pos.pos < end_pos)
		after_type_comments := field.comments[(before_comments.len + between_comments.len)..]
		// Handle comments before the field
		f.comments_before_field(before_comments)
		volatile_prefix := if field.is_volatile { 'volatile ' } else { '' }
		f.write('\t$volatile_prefix$field.name ')
		// Handle comments between field name and type
		before_len := f.line_len
		f.comments(between_comments, iembed: true, has_nl: false)
		comments_len := f.line_len - before_len
		mut field_align := field_aligns[field_align_i]
		if field_align.line_nr < field.pos.line_nr {
			field_align_i++
			field_align = field_aligns[field_align_i]
		}
		f.write(strings.repeat(` `, field_align.max_len - field.name.len - comments_len))
		f.write(field_types[i])
		f.mark_types_import_as_used(field.typ)
		attrs_len := inline_attrs_len(field.attrs)
		has_attrs := field.attrs.len > 0
		if has_attrs {
			f.write(strings.repeat(` `, field_align.max_type_len - field_types[i].len))
			f.single_line_attrs(field.attrs, inline: true)
		}
		if field.has_default_expr {
			mut align := default_expr_aligns[default_expr_align_i]
			if align.line_nr < field.pos.line_nr {
				default_expr_align_i++
				align = default_expr_aligns[default_expr_align_i]
			}
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
		// Handle comments after field type
		if after_type_comments.len > 0 {
			if after_type_comments[0].pos.line_nr > field.pos.line_nr {
				f.writeln('')
			} else {
				if !field.has_default_expr {
					mut align := comment_aligns[comment_align_i]
					if align.line_nr < field.pos.line_nr {
						comment_align_i++
						align = comment_aligns[comment_align_i]
					}
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
	f.comments_after_last_field(node.end_comments)
	f.writeln('}\n')
}

pub fn (mut f Fmt) struct_init(node ast.StructInit) {
	struct_init_save := f.is_struct_init
	f.is_struct_init = true
	defer {
		f.is_struct_init = struct_init_save
	}
	f.mark_types_import_as_used(node.typ)
	type_sym := f.table.sym(node.typ)
	// f.write('<old name: $type_sym.name>')
	mut name := type_sym.name
	if !name.starts_with('C.') && !name.starts_with('JS.') {
		name = f.no_cur_mod(f.short_module(type_sym.name)) // TODO f.type_to_str?
	}
	if name == 'void' {
		name = ''
	}
	if node.fields.len == 0 && !node.has_update_expr {
		// `Foo{}` on one line if there are no fields or comments
		if node.pre_comments.len == 0 {
			f.write('$name{}')
		} else {
			f.writeln('$name{')
			f.comments(node.pre_comments, inline: true, has_nl: true, level: .indent)
			f.write('}')
		}
		f.mark_import_as_used(name)
	} else if node.is_short {
		// `Foo{1,2,3}` (short syntax )
		f.write('$name{')
		f.mark_import_as_used(name)
		if node.has_update_expr {
			f.write('...')
			f.expr(node.update_expr)
			f.write(', ')
		}
		for i, field in node.fields {
			f.expr(field.expr)
			if i < node.fields.len - 1 {
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
		if !use_short_args {
			f.write('$name{')
			f.mark_import_as_used(name)
			if single_line_fields {
				f.write(' ')
			}
		}
		fields_start := f.out.len
		fields_loop: for {
			if !single_line_fields {
				if use_short_args && f.out[f.out.len - 1] == ` ` {
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
			f.comments(node.pre_comments, inline: true, has_nl: true, level: .keep)
			if node.has_update_expr {
				f.write('...')
				f.expr(node.update_expr)
				if single_line_fields {
					if node.fields.len > 0 {
						f.write(', ')
					}
				} else {
					f.writeln('')
				}
				f.comments(node.update_expr_comments, inline: true, has_nl: true, level: .keep)
			}
			for i, field in node.fields {
				f.write('$field.name: ')
				f.expr(field.expr)
				f.comments(field.comments, inline: true, has_nl: false, level: .indent)
				if single_line_fields {
					if i < node.fields.len - 1 {
						f.write(', ')
					}
				} else {
					f.writeln('')
				}
				f.comments(field.next_comments, inline: false, has_nl: true, level: .keep)
				if single_line_fields && (field.comments.len > 0
					|| field.next_comments.len > 0
					|| !expr_is_single_line(field.expr)
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
		if !use_short_args {
			if single_line_fields {
				f.write(' ')
			}
			f.write('}')
		}
	}
}
