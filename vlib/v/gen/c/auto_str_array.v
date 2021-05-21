module c

// Copyright (c) 2019-2021 Alexander Medvednikov. 2021 Dario Deledda. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import v.ast

fn (mut g Gen) gen_str_for_array(info ast.Array, styp string, str_fn_name string) {
	mut typ := info.elem_type
	mut sym := g.table.get_type_symbol(info.elem_type)
	if mut sym.info is ast.Alias {
		typ = sym.info.parent_type
		sym = g.table.get_type_symbol(typ)
	}
	field_styp := g.typ(typ)
	is_elem_ptr := typ.is_ptr()
	sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
	mut elem_str_fn_name := ''
	if sym_has_str_method {
		elem_str_fn_name = if is_elem_ptr {
			field_styp.replace('*', '') + '_str'
		} else {
			field_styp + '_str'
		}
		if sym.kind == .byte {
			elem_str_fn_name = elem_str_fn_name + '_escaped'
		}
	} else {
		elem_str_fn_name = styp_to_str_fn_name(field_styp)
	}
	if !sym_has_str_method {
		g.gen_str_for_type(typ)
	}
	g.type_definitions.writeln('static string ${str_fn_name}($styp a); // auto')
	g.auto_str_funcs.writeln('static string ${str_fn_name}($styp a) { return indent_${str_fn_name}(a, 0);}')
	g.type_definitions.writeln('static string indent_${str_fn_name}($styp a, int indent_count); // auto')
	g.auto_str_funcs.writeln('static string indent_${str_fn_name}($styp a, int indent_count) {')
	g.auto_str_funcs.writeln('\tstrings__Builder sb = strings__new_builder(a.len * 10);')
	g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, _SLIT("["));')
	g.auto_str_funcs.writeln('\tfor (int i = 0; i < a.len; ++i) {')
	if sym.kind == .function {
		g.auto_str_funcs.writeln('\t\tstring x = ${elem_str_fn_name}();')
	} else {
		if sym.kind == .array_fixed {
			g.auto_str_funcs.writeln('\t\t$field_styp it;')
			g.auto_str_funcs.writeln('\t\tmemcpy(*($field_styp*)it, (byte*)array_get(a, i), sizeof($field_styp));')
		} else {
			g.auto_str_funcs.writeln('\t\t$field_styp it = *($field_styp*)array_get(a, i);')
		}
		if should_use_indent_func(sym.kind) && !sym_has_str_method {
			if is_elem_ptr {
				g.auto_str_funcs.writeln('\t\tstring x = indent_${elem_str_fn_name}(*it, indent_count);')
			} else {
				g.auto_str_funcs.writeln('\t\tstring x = indent_${elem_str_fn_name}(it, indent_count);')
			}
		} else if sym.kind in [.f32, .f64] {
			if sym.kind == .f32 {
				g.auto_str_funcs.writeln('\t\tstring x = ${str_intp_g32('it')};')
			} else {
				g.auto_str_funcs.writeln('\t\tstring x = ${str_intp_g64('it')};')
			}

			//	g.auto_str_funcs.writeln('\t\tstring x = _STR("%g", 1, it);')
		} else if sym.kind == .rune {
			// Rune are managed at this level as strings
			g.auto_str_funcs.writeln('\t\tstring x = str_intp(2, (StrIntpData[]){{_SLIT("\`"), $si_s_code, {.d_s = ${elem_str_fn_name}(it) }}, {_SLIT("\`"), 0, {.d_c = 0 }}});')

			//	g.auto_str_funcs.writeln('\t\tstring x = _STR("`%.*s\\000`", 2, ${elem_str_fn_name}(it));')
		} else if sym.kind == .string {
			g.auto_str_funcs.writeln('\t\tstring x = str_intp(2, (StrIntpData[]){{_SLIT("\'"), $si_s_code, {.d_s = it }}, {_SLIT("\'"), 0, {.d_c = 0 }}});')

			//	g.auto_str_funcs.writeln('\t\tstring x = _STR("\'%.*s\\000\'", 2, it);')
		} else {
			// There is a custom .str() method, so use it.
			// NB: we need to take account of whether the user has defined
			// `fn (x T) str() {` or `fn (x &T) str() {`, and convert accordingly
			deref, deref_label := deref_kind(str_method_expects_ptr, is_elem_ptr, typ)
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _SLIT("$deref_label"));')
			g.auto_str_funcs.writeln('\t\tstring x = ${elem_str_fn_name}( $deref it);')
		}
	}
	g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, x);')
	if g.is_autofree && typ != ast.bool_type {
		// no need to free "true"/"false" literals
		g.auto_str_funcs.writeln('\t\tstring_free(&x);')
	}
	g.auto_str_funcs.writeln('\t\tif (i < a.len-1) {')
	g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write_string(&sb, _SLIT(", "));')
	g.auto_str_funcs.writeln('\t\t}')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, _SLIT("]"));')
	g.auto_str_funcs.writeln('\tstring res = strings__Builder_str(&sb);')
	g.auto_str_funcs.writeln('\tstrings__Builder_free(&sb);')
	g.auto_str_funcs.writeln('\treturn res;')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_array_fixed(info ast.ArrayFixed, styp string, str_fn_name string) {
	mut typ := info.elem_type
	mut sym := g.table.get_type_symbol(info.elem_type)
	if mut sym.info is ast.Alias {
		typ = sym.info.parent_type
		sym = g.table.get_type_symbol(typ)
	}
	field_styp := g.typ(typ)
	is_elem_ptr := typ.is_ptr()
	sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
	mut elem_str_fn_name := ''
	if sym_has_str_method {
		elem_str_fn_name = if is_elem_ptr {
			field_styp.replace('*', '') + '_str'
		} else {
			field_styp + '_str'
		}
	} else {
		elem_str_fn_name = styp_to_str_fn_name(field_styp)
	}
	if !sym.has_method('str') {
		elem_str_fn_name = g.gen_str_for_type(typ)
	}
	g.type_definitions.writeln('static string ${str_fn_name}($styp a); // auto')
	g.auto_str_funcs.writeln('static string ${str_fn_name}($styp a) { return indent_${str_fn_name}(a, 0);}')
	g.type_definitions.writeln('static string indent_${str_fn_name}($styp a, int indent_count); // auto')
	g.auto_str_funcs.writeln('static string indent_${str_fn_name}($styp a, int indent_count) {')
	g.auto_str_funcs.writeln('\tstrings__Builder sb = strings__new_builder($info.size * 10);')
	g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, _SLIT("["));')
	if sym.kind == .function {
		g.auto_str_funcs.writeln('\t\tstring x = ${elem_str_fn_name}();')
	} else {
		deref, deref_label := deref_kind(str_method_expects_ptr, is_elem_ptr, typ)
		g.auto_str_funcs.writeln('\tfor (int i = 0; i < $info.size; ++i) {')
		if should_use_indent_func(sym.kind) && !sym_has_str_method {
			if is_elem_ptr {
				g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _SLIT("$deref_label"));')
				g.auto_str_funcs.writeln('\t\tif ( 0 == a[i] ) {')
				g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write_string(&sb, _SLIT("0"));')
				g.auto_str_funcs.writeln('\t\t}else{')
				g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write_string(&sb, ${elem_str_fn_name}( $deref a[i]) );')
				g.auto_str_funcs.writeln('\t\t}')
			} else {
				g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${elem_str_fn_name}( $deref a[i]) );')
			}
		} else if sym.kind in [.f32, .f64] {
			if sym.kind == .f32 {
				g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${str_intp_g32('a[i]')} );')
			} else {
				g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${str_intp_g64('a[i]')} );')
			}
			// g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _STR("%g", 1, a[i]));')
		} else if sym.kind == .string {
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${str_intp_sq('a[i]')});')
			// g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _STR("\'%.*s\\000\'", 2, a[i]));')
		} else if sym.kind == .rune {
			tmp_str := str_intp_rune('${elem_str_fn_name}(  $deref a[i])')
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, $tmp_str);')
			// g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _STR("`%.*s\\000`", 2, ${elem_str_fn_name}(a[i])));')
		} else {
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${elem_str_fn_name}( $deref a[i]));')
		}
	}
	g.auto_str_funcs.writeln('\t\tif (i < ${info.size - 1}) {')
	g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write_string(&sb, _SLIT(", "));')
	g.auto_str_funcs.writeln('\t\t}')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, _SLIT("]"));')
	g.auto_str_funcs.writeln('\tstring res = strings__Builder_str(&sb);')
	g.auto_str_funcs.writeln('\tstrings__Builder_free(&sb);')
	g.auto_str_funcs.writeln('\treturn res;')
	g.auto_str_funcs.writeln('}')
}
