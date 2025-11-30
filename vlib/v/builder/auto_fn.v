module builder

import strings
import v.ast
import v.parser

pub fn (mut b Builder) gen_auto_fn() {
	b.gen_auto_str_fn()
}

fn (mut b Builder) gen_auto_str_fn() {
	// construct a `type_map`, to generate only one file for a module
	mut need_auto_str_fn_type_map := map[string][]&ast.TypeSymbol{}
	for s in b.table.type_symbols {
		has_str_method := s.has_method_with_sumtype_parent('str')
			|| s.has_method_with_generic_parent('str')
		if !s.need_str_fn || has_str_method || s.kind == .function || s.mod == '' {
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
			for t in types {
				type_name := t.name.all_after_last('.')
				match t.kind {
					.struct {
						info := t.info as ast.Struct
						gen_auto_str_fn_struct(mut sb, type_name, info)
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

fn gen_auto_str_fn_struct(mut sb strings.Builder, struct_name string, info ast.Struct) {
	clean_struct_v_type_name := if info.is_anon { 'struct ' } else { struct_name }
	// find `[str: skip]` fields
	mut field_skips := []int{}
	for i, field in info.fields {
		if attr := field.attrs.find_first('str') {
			if attr.arg == 'skip' {
				field_skips << i
			}
		}
	}
	sb.write_string("
@[markused]
pub fn (it ${struct_name})str() string {
		return '${clean_struct_v_type_name}{")
	if info.fields.len > 0 {
		for i, f in info.fields {
			// Skip `str:skip` fields
			if i in field_skips {
				continue
			}
			if f.typ == ast.string_type {
				sb.writeln('    ${f.name}: \\\'\${it.${f.name}}\\\'')
			} else {
				sb.writeln('\n    ${f.name}: \${it.${f.name}}')
			}
		}
	}
	sb.writeln("}'
}
")
}

fn (b Builder) get_mod_full_path(full_mod_name string) string {
	for f in b.parsed_files {
		if f.mod.name == full_mod_name {
			return f.path.all_before_last('/')
		}
	}
	return ''
}
