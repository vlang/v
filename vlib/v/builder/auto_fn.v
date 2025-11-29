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
			mut the_file := parser.parse_text(v_file, '__v_auto_generated_auto_fn(${full_mod_name}).v', mut
				b.table, .skip_comments, b.pref)
			b.parsed_files << the_file
			// need to call `checker` for this generated v_file
			b.checker.check(mut the_file)
		}
	}
}

fn gen_auto_str_fn_struct(mut sb strings.Builder, struct_name string, info ast.Struct) {
	sb.writeln("
@[markused]
pub fn (it ${struct_name})str() string {
	return 'struct ${struct_name} { ")
	for f in info.fields {
		sb.writeln('\t\t${f.name}: \${it.${f.name}}')
	}
	sb.writeln("\t}'
}
")
}
