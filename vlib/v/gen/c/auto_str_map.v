module c

import v.ast
//import v.util

fn (mut g Gen) gen_str_for_map(info ast.Map, styp string, str_fn_name string) {
	mut key_typ := info.key_type
	mut key_sym := g.table.get_type_symbol(key_typ)
	if mut key_sym.info is ast.Alias {
		key_typ = key_sym.info.parent_type
		key_sym = g.table.get_type_symbol(key_typ)
	}
	key_styp := g.typ(key_typ)
	key_str_fn_name := key_styp.replace('*', '') + '_str'
	if !key_sym.has_method('str') {
		g.gen_str_for_type(key_typ)
	}

	mut val_typ := info.value_type
	mut val_sym := g.table.get_type_symbol(val_typ)
	if mut val_sym.info is ast.Alias {
		val_typ = val_sym.info.parent_type
		val_sym = g.table.get_type_symbol(val_typ)
	}
	val_styp := g.typ(val_typ)
	elem_str_fn_name := val_styp.replace('*', '') + '_str'
	if !val_sym.has_method('str') {
		g.gen_str_for_type(val_typ)
	}

	g.type_definitions.writeln('static string ${str_fn_name}($styp m); // auto')
	g.auto_str_funcs.writeln('static string ${str_fn_name}($styp m) { return indent_${str_fn_name}(m, 0);}')
	g.type_definitions.writeln('static string indent_${str_fn_name}($styp m, int indent_count); // auto')
	g.auto_str_funcs.writeln('static string indent_${str_fn_name}($styp m, int indent_count) { /* gen_str_for_map */')
	g.auto_str_funcs.writeln('\tstrings__Builder sb = strings__new_builder(m.key_values.len*10);')
	g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, _SLIT("{"));')
	g.auto_str_funcs.writeln('\tfor (int i = 0; i < m.key_values.len; ++i) {')
	g.auto_str_funcs.writeln('\t\tif (!DenseArray_has_index(&m.key_values, i)) { continue; }')

	if key_sym.kind == .string {
		g.auto_str_funcs.writeln('\t\tstring key = *(string*)DenseArray_key(&m.key_values, i);')
	} else {
		g.auto_str_funcs.writeln('\t\t$key_styp key = *($key_styp*)DenseArray_key(&m.key_values, i);')
	}
	if key_sym.kind == .string {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${str_intp_sq("key")});')
		//g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _STR("\'%.*s\\000\'", 2, key));')
	} else if key_sym.kind == .rune {
		tmp_str := str_intp_rune("${key_str_fn_name}(key)")
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${tmp_str});')
		//g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _STR("`%.*s\\000`", 2, ${key_str_fn_name}(key)));')
	} else {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${key_str_fn_name}(key));')
	}
	g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _SLIT(": "));')
	if val_sym.kind == .function {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${elem_str_fn_name}());')
	} else if val_sym.kind == .string {
		tmp_str := str_intp_sq("*($val_styp*)DenseArray_value(&m.key_values, i)")
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${tmp_str});')
		//g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _STR("\'%.*s\\000\'", 2, *($val_styp*)DenseArray_value(&m.key_values, i)));')
	} else if should_use_indent_func(val_sym.kind) && !val_sym.has_method('str') {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, indent_${elem_str_fn_name}(*($val_styp*)DenseArray_value(&m.key_values, i), indent_count));')
	} else if val_sym.kind in [.f32, .f64] {
		
		tmp_val := "*($val_styp*)DenseArray_value(&m.key_values, i)"
		if val_sym.kind == .f32 {
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${str_intp_g32(tmp_val)});')
		} else {
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${str_intp_g64(tmp_val)});')
		}
		//g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _STR("%g", 1, *($val_styp*)DenseArray_value(&m.key_values, i)));')
	
	} else if val_sym.kind == .rune {
		tmp_str := str_intp_rune("${elem_str_fn_name}(*($val_styp*)DenseArray_value(&m.key_values, i))")
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${tmp_str});')
		//g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _STR("`%.*s\\000`", 2, ${elem_str_fn_name}(*($val_styp*)DenseArray_value(&m.key_values, i))));')
	} else {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${elem_str_fn_name}(*($val_styp*)DenseArray_value(&m.key_values, i)));')
	}
	g.auto_str_funcs.writeln('\t\tif (i != m.key_values.len-1) {')
	g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write_string(&sb, _SLIT(", "));')
	g.auto_str_funcs.writeln('\t\t}')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, _SLIT("}"));')
	g.auto_str_funcs.writeln('\tstring res = strings__Builder_str(&sb);')
	g.auto_str_funcs.writeln('\tstrings__Builder_free(&sb);')
	g.auto_str_funcs.writeln('\treturn res;')
	g.auto_str_funcs.writeln('}')
}