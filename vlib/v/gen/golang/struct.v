// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module golang

// import strings
import v.ast
import v.mathutil as mu

pub fn (mut f Gen) struct_decl(node ast.StructDecl) {
	f.attrs(node.attrs)
	f.write('type ')
	if node.is_pub {
		f.write('pub ')
	}
	f.write_language_prefix(node.language)
	name := node.name.after('.') // strip prepended module
	f.write(name)
	if node.is_union {
		f.write('union ')
	} else {
		f.write(' struct ')
	}
	f.write_generic_types(node.generic_types)
	if node.fields.len == 0 && node.embeds.len == 0 && node.pos.line_nr == node.pos.last_line {
		f.writeln(' {}')
		return
	}
	mut field_types := []string{cap: node.fields.len}
	for field in node.fields {
		ft := f.no_cur_mod(f.table.type_to_str_using_aliases(field.typ, f.mod2alias))
		field_types << ft
		// attrs_len := inline_attrs_len(field.attrs)
		// end_pos := field.pos.pos + field.pos.len
		// mut comments_len := 0 // Length of comments between field name and type
		// field_aligns.add_info(comments_len + field.name.len, ft.len, field.pos.line_nr)
		if field.has_default_expr {
			// default_expr_aligns.add_info(attrs_len, field_types[i].len, field.pos.line_nr,
			// use_threshold: true)
		}
	}
	f.writeln(' {')
	for embed in node.embeds {
		f.mark_types_import_as_used(embed.typ)
		styp := f.table.type_to_str_using_aliases(embed.typ, f.mod2alias)
		f.writeln('\t${styp}')
	}
	// mut field_align_i := 0
	// mut comment_align_i := 0
	// mut default_expr_align_i := 0
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
			if node.fields[i - 1].has_default_expr {
				before_last_line = mu.max(before_last_line, node.fields[i - 1].default_expr.pos().last_line)
			}

			mut next_first_line := field.pos.line_nr
			if next_first_line - before_last_line > 1 {
				f.writeln('')
			}
		}
		// end_pos := field.pos.pos + field.pos.len
		volatile_prefix := if field.is_volatile { 'volatile ' } else { '' }
		f.write('\t${volatile_prefix}${field.name} ')
		// before_len := f.line_len
		// mut field_align := field_aligns[field_align_i]
		// if field_align.line_nr < field.pos.line_nr {
		// field_align_i++
		// field_align = field_aligns[field_align_i]
		//}
		// f.write(strings.repeat(` `, field_align.max_len - field.name.len - comments_len))
		f.write(field_types[i])
		f.mark_types_import_as_used(field.typ)
		// attrs_len := inline_attrs_len(field.attrs)
		has_attrs := field.attrs.len > 0
		if has_attrs {
			// f.write(strings.repeat(` `, field_align.max_type_len - field_types[i].len))
			f.single_line_attrs(field.attrs, inline: true)
		}
		if field.has_default_expr {
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
		f.writeln('')
	}
	f.writeln('}\n')
}

pub fn (mut f Gen) struct_init(node ast.StructInit) {
	struct_init_save := f.is_struct_init
	f.is_struct_init = true
	defer {
		f.is_struct_init = struct_init_save
	}

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
			f.write('${name}{}')
		} else {
			f.writeln('${name}{')
			f.write('}')
		}
		f.mark_import_as_used(name)
	} else if node.no_keys {
		// `Foo{1,2,3}` (short syntax )
		f.write('${name}{')
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
			f.write('${name}{')
			f.mark_import_as_used(name)
			if single_line_fields {
				f.write(' ')
			}
		}
		// fields_start := f.out.len
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
			}
			for i, field in node.fields {
				f.write('${field.name}: ')
				f.expr(field.expr)
				if single_line_fields {
					if i < node.fields.len - 1 {
						f.write(', ')
					}
				} else {
					f.writeln('')
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
