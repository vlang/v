module builder

import strings
import v.ast
import v.parser
import os

pub fn (mut b Builder) gen_auto_fn() {
	b.gen_auto_str_fn()
}

fn (mut b Builder) gen_auto_str_fn() {
	mut generic_to_concrete_map := map[ast.Type][]ast.Type{}
	// construct a `type_map`, to generate only one file for a module
	mut need_auto_str_fn_type_map := map[string][]&ast.TypeSymbol{}
	for t, _ in b.table.used_features.used_str {
		if t == ast.no_type || t == ast.void_type {
			continue
		}
		s := b.table.sym(t)
		has_str_method := s.has_method_with_sumtype_parent('str')
			|| s.has_method_with_generic_parent('str')
		if has_str_method || s.kind == .function || s.mod == '' {
			continue
		} else {
			unsafe {
				if s !in need_auto_str_fn_type_map[s.mod] {
					need_auto_str_fn_type_map[s.mod] << s
				}
			}
		}
	}

	if need_auto_str_fn_type_map.len > 0 {
		mut sb := strings.new_builder(128)
		mut check_file_list := []&ast.File{}
		for full_mod_name, types in need_auto_str_fn_type_map {
			mod_name := full_mod_name.all_after_last('.')
			sb.writeln('module ${mod_name}')
			b.auto_str_has_import_strings = false
			for t in types {
				mut type_name := b.auto_fn_get_type_name(t, false) // main.MyS[int] => MyS[T]
				if full_mod_name != 'builtin' {
					type_name = full_mod_name + '.' + type_name
				}
				type_name2 := b.auto_fn_get_type_name(t, true) // main.MyS[int] => MyS[${T.name}]
				match t.kind {
					.struct {
						info := t.info as ast.Struct
						if info.parent_type.has_flag(.generic) {
							// for `post_process_generic_fns`
							generic_to_concrete_map[info.parent_type] << info.concrete_types
						}
						hint := 'type=${t.idx}'
						b.gen_str_for_struct(mut sb, type_name, type_name2, info, hint)
					}
					else {}
				}
			}
			v_file := sb.str()
			v_file_name := b.get_mod_full_path(full_mod_name) +
				'/v_auto_generated_auto_fn(${full_mod_name}).v'
			// os.write_file(v_file_name, v_file) or {}
			dump(v_file_name)
			dump(v_file)
			mut the_file := parser.parse_text(v_file, v_file_name, mut b.table, .skip_comments,
				b.pref)
			// mut the_file := parser.parse_file(v_file_name, mut b.table, .skip_comments, b.pref)
			// os.rm(v_file_name) or {}
			// the_file.is_parse_text = false
			b.parsed_files << the_file
			b.table.filelist << v_file_name
			check_file_list << the_file
		}
		// need to call `checker` for this generated v_file
		for mut f in check_file_list {
			b.checker.check(mut f)
			// dump(b.checker.table.fn_generic_types)
			b.auto_fn_fix_generics(mut f, generic_to_concrete_map)
		}
	}
}

fn (mut b Builder) gen_str_for_struct(mut sb strings.Builder, struct_name string, struct_name2 string, info ast.Struct, hint string) {
	sb.writeln('/*${hint}*/')
	clean_struct_v_type_name := if info.is_anon { 'struct ' } else { struct_name2 }
	generic_types := if info.generic_types.len > 0 {
		'[' + info.generic_types.map(b.table.sym(it).name).join(',') + ']'
	} else {
		''
	}
	if info.fields.len == 0 {
		sb.writeln("@[markused]\npub fn (it ${struct_name}) str${generic_types}() string { return '${clean_struct_v_type_name}{}' }")
		sb.writeln("@[markused]\npub fn (it ${struct_name}) indent_str${generic_types}(_ int) string { return '${clean_struct_v_type_name}{}' }")
		return
	}
	// -hide-auto-str hides potential sensitive struct data from resulting binary files
	if b.pref.hide_auto_str {
		sb.writeln("@[markused]\npub fn (it ${struct_name}) str${generic_types}() string { return 'str() used with -hide-auto-str' }")
		sb.writeln("@[markused]\npub fn (it ${struct_name}) indent_str${generic_types}(_ int) string { return 'str() used with -hide-auto-str' }")
		return
	}
	if !b.auto_str_has_import_strings {
		b.auto_str_has_import_strings = true
		sb.writeln('import strings\n')
	}
	sb.writeln('@[markused]\npub fn (it ${struct_name}) str${generic_types}() string { return it.indent_str(0) }')
	sb.writeln('@[markused]\npub fn (it ${struct_name}) indent_str${generic_types}(indent_count int) string {')
	// is_c_struct := struct_name.starts_with('C.')
	if b.pref.hide_auto_str {
		sb.write_string("\treturn 'str() used with -hide-auto-str'\n}")
		return
	}
	sb.writeln('\tmut res := strings.new_builder(128)')
	sb.writeln("\tindents := '    '.repeat(indent_count)")
	sb.writeln('\tres.write_string(\'${clean_struct_v_type_name}{\')')
	// find `[str: skip]` fields
	mut field_skips := []int{}
	for i, field in info.fields {
		if attr := field.attrs.find_first('str') {
			if attr.arg == 'skip' {
				field_skips << i
			}
		}
	}
	mut is_first := true
	for i, f in info.fields {
		// Skip `str:skip` fields
		if i in field_skips {
			continue
		}
		if is_first {
			is_first = false
			sb.writeln("\tres.write_string('\\n')")
		}
		if f.typ.has_flag(.shared_f) {
			sb.writeln('\trlock it.${f.name} {')
		}
		if f.typ == ast.string_type {
			sb.writeln('\tres.write_string(\'\${indents}    ${f.name}: \\\'\${it.${f.name}}\\\'\\n\')')
		} else {
			sym := b.table.sym(f.typ)
			if sym.kind == .struct {
				if f.typ.has_flag(.option) {
					sb.writeln('\tif option_f := it.${f.name} {')
					sb.writeln("\t\tres.write_string('\${indents}    ${f.name}: Option(\${option_f.indent_str(indent_count+1)})\\n')")
					sb.writeln('\t}\n\telse {')
					sb.writeln("\t\tres.write_string('\${indents}    ${f.name}: Option(none)\\n')")
					sb.writeln('\t}')
				} else {
					sb.writeln("\tres.write_string('\${indents}    ${f.name}: \${it.${f.name}.indent_str(indent_count+1)}\\n')")
				}
			} else {
				sb.writeln("\tres.write_string('\${indents}    ${f.name}: \${it.${f.name}}\\n')")
			}
		}
		if f.typ.has_flag(.shared_f) {
			sb.writeln('\t}')
		}
	}
	sb.writeln("\tres.write_string('\${indents}}')")
	sb.writeln('\treturn res.str()\n}')
}

fn (b Builder) get_mod_full_path(full_mod_name string) string {
	for f in b.parsed_files {
		if f.mod.name == full_mod_name {
			return os.dir(f.path)
		}
	}
	return ''
}

fn (mut b Builder) auto_fn_get_type_name(t &ast.TypeSymbol, is_get_type_name bool) string {
	mut type_name := t.name
	if t.kind == .struct {
		// main.MyS[int] => MyS[T]
		info := t.info as ast.Struct
		type_name = b.table.clean_generics_type_str(t.idx).all_after_last('.')
		if info.generic_types.len > 0 {
			type_name += '['
			generic_names := info.generic_types.map(b.table.sym(it).name)
			if is_get_type_name {
				type_name += '\${'
				type_name += generic_names.join('.name},\${')
				type_name += '.name}'
			} else {
				type_name += generic_names.join(',')
			}
			type_name += ']'
		}
	}
	return type_name
}

fn (mut b Builder) auto_fn_fix_generics(mut file ast.File, generic_to_concrete_map map[ast.Type][]ast.Type) {
	if file.generic_fns.len == 0 {
		return
	}
	for node in file.generic_fns {
		if concrete_types := generic_to_concrete_map[node.receiver.typ] {
			fkey := node.fkey()
			b.table.fn_generic_types[fkey] << concrete_types
		}
	}
	b.checker.change_current_file(file)
	b.checker.post_process_generic_fns() or {}
}
