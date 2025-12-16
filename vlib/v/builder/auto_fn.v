module builder

import strings
import v.ast
import v.parser
import os

pub fn (mut b Builder) gen_auto_fn() {
	b.gen_auto_str_fn()
}

fn (mut b Builder) gen_auto_str_fn() {
	mut generic_to_concrete_map := map[ast.Type][][]ast.Type{}
	// construct a `type_map`, to generate only one file for a module
	mut need_auto_str_fn_type_map := map[string][]&ast.TypeSymbol{}
	mut already_gen_str_map := map[string]bool{}
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
			sb.writeln('module ${mod_name}\nimport strings')
			for t in types {
				mut type_name := b.auto_fn_get_type_name(t, false) // main.MyS[int] => MyS[T]
				if t.name.starts_with('C.') {
					type_name = 'C.' + type_name
				} else if full_mod_name != 'builtin' {
					type_name = full_mod_name + '.' + type_name
				}
				type_name2 := b.auto_fn_get_type_name(t, true) // main.MyS[int] => MyS[${T.name}]
				match t.kind {
					.struct {
						info := t.info as ast.Struct
						if info.parent_type.has_flag(.generic) {
							// for `post_process_generic_fns`
							generic_to_concrete_map[info.parent_type] << [
								info.concrete_types,
							]
						}
						hint := 'type=${t.idx}\nt.name=${t.name}'
						if type_name !in already_gen_str_map {
							b.gen_str_for_struct(mut sb, type_name, type_name2, info,
								hint)
							already_gen_str_map[type_name] = true
						}
					}
					.alias {
						info := t.info as ast.Alias
						hint := 'type=${t.idx}\nt.name=${t.name}'
						if type_name !in already_gen_str_map {
							b.gen_str_for_alias(mut sb, type_name, type_name2, info, hint)
							already_gen_str_map[type_name] = true
						}
					}
					else {}
				}
			}
			v_file := sb.str()
			v_file_name := b.get_mod_full_path(full_mod_name) +
				'/v_auto_generated_auto_fn(${full_mod_name}).v'
			// os.write_file(v_file_name, v_file) or {}
			$if trace_auto_fn ? {
				dump(v_file_name)
				dump(v_file)
			}
			mut the_file := parser.parse_text(v_file, v_file_name, mut b.table, .skip_comments,
				b.pref)
			b.parsed_files << the_file
			b.table.filelist << v_file_name
			check_file_list << the_file
		}
		// need to call `checker` for this generated v_file
		for mut f in check_file_list {
			b.checker.check(mut f)
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
		return
	}
	// -hide-auto-str hides potential sensitive struct data from resulting binary files
	if b.pref.hide_auto_str {
		sb.writeln("@[markused]\npub fn (it ${struct_name}) str${generic_types}() string { return 'str() used with -hide-auto-str' }")
		return
	}
	sb.writeln('@[markused]\npub fn (it ${struct_name}) str${generic_types}() string {')
	// is_c_struct := struct_name.starts_with('C.')
	sb.writeln('\tmut res := strings.new_builder(222)')
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
			sb.writeln("\tres.writeln('')")
		}
		if f.typ.has_flag(.shared_f) {
			sb.writeln('\trlock it.${f.name} {')
		}
		if f.typ == ast.string_type {
			sb.writeln('\tres.write_string(\'${f.name}: \\\'\${it.${f.name}}\\\'\\n\')')
		} else {
			sym := b.table.sym(f.typ)
			if sym.kind == .struct {
				if f.typ.has_flag(.option) {
					sb.writeln('\tif option_f := it.${f.name} {')
					sb.writeln("\t\tres.writeln('${f.name}: Option(\${option_f})')")
					sb.writeln('\t}\n\telse {')
					sb.writeln("\t\tres.writeln('${f.name}: Option(none)')")
					sb.writeln('\t}')
				} else {
					sb.writeln("\tres.writeln('${f.name}: \${it.${f.name}}')")
				}
			} else if sym.kind == .chan {
				elem_info := sym.info as ast.Chan
				elem_type_str := b.table.type_to_str(elem_info.elem_type)
				sb.writeln("\tres.writeln('${f.name}: chan ${elem_type_str}{cap: \${it.${f.name}.cap}, closed: \${it.${f.name}.closed}}')")
			} else {
				sb.writeln("\tres.writeln('${f.name}: \${it.${f.name}}')")
			}
		}
		if f.typ.has_flag(.shared_f) {
			sb.writeln('\t}')
		}
	}
	sb.writeln("\tres.write_string('}')")
	sb.writeln('\t// note: this can be removed after move dump beyond cgen')
	sb.writeln('\tmut sb := strings.new_builder(222)')
	sb.writeln('\tsb.indent(res.str())')
	sb.writeln('\treturn sb.str()\n}')
}

fn (mut b Builder) gen_str_for_alias(mut sb strings.Builder, alias_name string, alias_name2 string, info ast.Alias, hint string) {
	sb.writeln('/*${hint}*/')
	// -hide-auto-str hides potential sensitive struct data from resulting binary files
	if b.pref.hide_auto_str {
		sb.writeln("@[markused]\npub fn (it ${alias_name}) str() string { return 'str() used with -hide-auto-str' }")
		return
	}
	parent_type_name := b.table.type_to_str(info.parent_type)
	sb.writeln('@[markused]\npub fn (it ${alias_name}) str() string {')
	sb.writeln('\tparent_it := *(&${parent_type_name}(&it))')
	sb.writeln('\tmut res := strings.new_builder(222)')
	sb.writeln("\tres.write_string('    ${alias_name2}(\${parent_it})')")
	// sb.writeln('\t// note: this can be removed after move dump beyond cgen')
	// sb.writeln('\tmut sb := strings.new_builder(222)')
	// sb.writeln('\tsb.indent(res.str())')
	// sb.writeln('\treturn sb.str()\n}')
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
	mut type_name := t.name.all_after_last('.')
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

fn (mut b Builder) auto_fn_fix_generics(mut file ast.File, generic_to_concrete_map map[ast.Type][][]ast.Type) {
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
