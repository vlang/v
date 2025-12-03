module builder

import strings
import v.ast
import v.parser
import os

pub fn (mut b Builder) gen_auto_fn() {
	b.gen_auto_str_fn()
}

fn (mut b Builder) gen_auto_str_fn() {
	// construct a `type_map`, to generate only one file for a module
	mut need_auto_str_fn_type_map := map[string][]&ast.TypeSymbol{}
	for t, _ in b.table.used_features.used_str {
		if t == ast.no_type || t == ast.void_type {
			continue
		}
		s := b.table.sym(t)
		has_str_method := s.has_method_with_sumtype_parent('str')
			|| s.has_method_with_generic_parent('str')
		if has_str_method || s.kind == .function || s.mod == '' || s.mod == 'builtin' {
			continue
		} else {
			unsafe { need_auto_str_fn_type_map[s.mod] << s }
		}
	}

	if need_auto_str_fn_type_map.len > 0 {
		mut sb := strings.new_builder(128)
		for full_mod_name, types in need_auto_str_fn_type_map {
			mod_name := full_mod_name.all_after_last('.')
			sb.writeln('module ${mod_name}')
			b.auto_str_has_import_strings = false
			for t in types {
				type_name := t.name.all_after_last('.')
				match t.kind {
					.struct {
						info := t.info as ast.Struct
						b.gen_str_for_struct(mut sb, type_name, info)
					}
					else {}
				}
			}
			v_file := sb.str()
			v_file_name := b.get_mod_full_path(full_mod_name) +
				'/__v_auto_generated_auto_fn(${full_mod_name}).v'
			mut the_file := parser.parse_text(v_file, v_file_name, mut b.table, .skip_comments,
				b.pref)
			b.parsed_files << the_file
			b.table.filelist << v_file_name
			// need to call `checker` for this generated v_file
			b.checker.check(mut the_file)
		}
	}
}

fn (mut b Builder) gen_str_for_struct(mut sb strings.Builder, struct_name string, info ast.Struct) {
	clean_struct_v_type_name := if info.is_anon { 'struct ' } else { struct_name }
	if info.fields.len == 0 {
		sb.writeln("@[markused]\npub fn (it ${struct_name}) str() string { return '${clean_struct_v_type_name}{}'}")
		sb.writeln("@[markused]\npub fn (it ${struct_name}) indent_str(_ int) string { return '${clean_struct_v_type_name}{}'}")
		return
	}
	// -hide-auto-str hides potential sensitive struct data from resulting binary files
	if b.pref.hide_auto_str {
		sb.writeln("@[markused]\npub fn (it ${struct_name}) str() string { return 'str() used with -hide-auto-str'}")
		sb.writeln("@[markused]\npub fn (it ${struct_name}) indent_str(_ int) string { return 'str() used with -hide-auto-str'}")
		return
	}
	if !b.auto_str_has_import_strings {
		b.auto_str_has_import_strings = true
		sb.writeln('import strings\n')
	}
	sb.writeln('@[markused]\npub fn (it ${struct_name}) str() string { return it.indent_str(0) }')
	sb.writeln('@[markused]\npub fn (it ${struct_name}) indent_str(indent_count int) string {')
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
				sb.writeln("\tres.write_string('\${indents}    ${f.name}: \${it.${f.name}.indent_str(indent_count+1)}\\n')")
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
