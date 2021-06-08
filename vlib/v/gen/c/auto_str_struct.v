module c

// Copyright (c) 2019-2021 Alexander Medvednikov. 2021 Dario Deledda. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import v.ast
import v.util

fn (g &Gen) type_to_fmt1(typ ast.Type) StrIntpType {
	if typ == ast.byte_type_idx {
		return .si_u8
	}
	if typ == ast.char_type_idx {
		return .si_c
	}
	if typ == ast.voidptr_type_idx || typ in ast.byteptr_types {
		return .si_p
	}
	if typ in ast.charptr_types {
		return .si_s
		// return '%C\\000' // a C string
	}
	sym := g.table.get_type_symbol(typ)
	if typ.is_ptr() && (typ.is_int_valptr() || typ.is_float_valptr()) {
		return .si_s
	} else if sym.kind in [.struct_, .array, .array_fixed, .map, .bool, .enum_, .interface_,
		.sum_type, .function, .alias, .chan] {
		return .si_s
	} else if sym.kind == .string {
		return .si_s
		// return "'%.*s\\000'"
	} else if sym.kind in [.f32, .f64] {
		if sym.kind == .f32 {
			return .si_g32
		}
		return .si_g64
	} else if sym.kind == .int {
		return .si_i32
	} else if sym.kind == .u32 {
		return .si_u32
	} else if sym.kind == .u64 {
		return .si_u64
	} else if sym.kind == .i64 {
		return .si_i64
	}
	return .si_i32
}

fn (mut g Gen) gen_str_for_struct(info ast.Struct, styp string, str_fn_name string) {
	// g.gen_str_for_struct1(info, styp, str_fn_name)

	// TODO: short it if possible
	// generates all definitions of substructs
	mut fnames2strfunc := map{
		'': ''
	}
	for field in info.fields {
		sym := g.table.get_type_symbol(field.typ)
		if !sym.has_method('str') {
			mut typ := field.typ
			if typ.is_ptr() {
				typ = typ.deref()
			}
			field_styp := g.typ(typ)
			field_fn_name := g.gen_str_for_type(field.typ)
			fnames2strfunc[field_styp] = field_fn_name
		}
	}
	// _str() functions should have a single argument, the indenting ones take 2:
	g.type_definitions.writeln('static string ${str_fn_name}($styp it); // auto')
	g.auto_str_funcs.writeln('static string ${str_fn_name}($styp it) { return indent_${str_fn_name}(it, 0);}')
	g.type_definitions.writeln('static string indent_${str_fn_name}($styp it, int indent_count); // auto')
	g.auto_str_funcs.writeln('static string indent_${str_fn_name}($styp it, int indent_count) {')
	mut clean_struct_v_type_name := styp.replace('__', '.')
	if clean_struct_v_type_name.contains('_T_') {
		// TODO: this is a bit hacky. styp shouldn't be even parsed with _T_
		// use something different than g.typ for styp
		clean_struct_v_type_name =
			clean_struct_v_type_name.replace('Array_', '[]').replace('_T_', '<').replace('_', ', ') +
			'>'
	}
	clean_struct_v_type_name = util.strip_main_name(clean_struct_v_type_name)
	// generate ident / indent length = 4 spaces
	if info.fields.len == 0 {
		g.auto_str_funcs.writeln('\treturn _SLIT("$clean_struct_v_type_name{}");')
		g.auto_str_funcs.writeln('}')
		g.auto_str_funcs.writeln('')
		return
	}

	g.auto_str_funcs.writeln('\tstring indents = string_repeat(_SLIT("    "), indent_count);')
	g.auto_str_funcs.writeln('\tstring res = str_intp( ${info.fields.len * 4 + 3}, _MOV((StrIntpData[]){')
	g.auto_str_funcs.writeln('\t\t{_SLIT("$clean_struct_v_type_name{\\n"), 0, {.d_c=0}},')

	for i, field in info.fields {
		mut ptr_amp := if field.typ.is_ptr() { '&' } else { '' }
		base_fmt := g.type_to_fmt1(g.unwrap_generic(field.typ))

		// manage prefix and quote symbol for the filed
		mut quote_str := ''
		mut prefix := ''
		sym := g.table.get_type_symbol(g.unwrap_generic(field.typ))
		if sym.kind == .string {
			quote_str = "'"
		} else if field.typ in ast.charptr_types {
			quote_str = '\\"'
			prefix = 'C'
		}

		// first fields doesn't need \n
		if i == 0 {
			g.auto_str_funcs.write_string('\t\t{_SLIT0, $si_s_code, {.d_s=indents}}, {_SLIT("    $field.name: $ptr_amp$prefix"), 0, {.d_c=0}}, ')
		} else {
			g.auto_str_funcs.write_string('\t\t{_SLIT("\\n"), $si_s_code, {.d_s=indents}}, {_SLIT("    $field.name: $ptr_amp$prefix"), 0, {.d_c=0}}, ')
		}

		// custom methods management
		has_custom_str := sym.has_method('str')
		mut field_styp := g.typ(field.typ).replace('*', '')
		field_styp_fn_name := if has_custom_str {
			'${field_styp}_str'
		} else {
			fnames2strfunc[field_styp]
		}

		// manage the fact hat with float we use always the g representation
		if sym.kind !in [.f32, .f64] {
			g.auto_str_funcs.write_string('{_SLIT("$quote_str"), ${int(base_fmt)}, {.${data_str(base_fmt)}=')
		} else {
			g_fmt := '0x' + (u32(base_fmt) | u32(0x7F) << 9).hex()
			g.auto_str_funcs.write_string('{_SLIT("$quote_str"), $g_fmt, {.${data_str(base_fmt)}=')
		}

		mut func := struct_auto_str_func1(sym, field.typ, field_styp_fn_name, field.name)

		// manage reference types can be "nil"
		if field.typ.is_ptr() && !(field.typ in ast.charptr_types
			|| field.typ in ast.byteptr_types || field.typ == ast.voidptr_type_idx) {
			g.auto_str_funcs.write_string('isnil(it.${c_name(field.name)})')
			g.auto_str_funcs.write_string(' ? _SLIT("nil") : ')
			// struct, floats and ints have a special case through the _str function
			if sym.kind != .struct_ && !field.typ.is_int_valptr() && !field.typ.is_float_valptr() {
				g.auto_str_funcs.write_string('*')
			}
		}
		// handle circular ref type of struct to the struct itself
		if styp == field_styp {
			g.auto_str_funcs.write_string('_SLIT("<circular>")')
		} else {
			// manage C charptr
			if field.typ in ast.charptr_types {
				g.auto_str_funcs.write_string('tos2((byteptr)$func)')
			} else {
				g.auto_str_funcs.write_string(func)
			}
		}

		g.auto_str_funcs.writeln('}}, {_SLIT("$quote_str"), 0, {.d_c=0}},')
	}
	g.auto_str_funcs.writeln('\t\t{_SLIT("\\n"), $si_s_code, {.d_s=indents}}, {_SLIT("}"), 0, {.d_c=0}},')
	g.auto_str_funcs.writeln('\t}));')
	g.auto_str_funcs.writeln('\tstring_free(&indents);')
	g.auto_str_funcs.writeln('\treturn res;')
	g.auto_str_funcs.writeln('}')
}

fn struct_auto_str_func1(sym &ast.TypeSymbol, field_type ast.Type, fn_name string, field_name string) string {
	has_custom_str, expects_ptr, _ := sym.str_method_info()
	if sym.kind == .enum_ {
		return '${fn_name}(it.${c_name(field_name)})'
	} else if should_use_indent_func(sym.kind) {
		mut obj := 'it.${c_name(field_name)}'
		if field_type.is_ptr() && !expects_ptr {
			obj = '*$obj'
		}
		if has_custom_str {
			return '${fn_name}($obj)'
		}
		return 'indent_${fn_name}($obj, indent_count + 1)'
	} else if sym.kind in [.array, .array_fixed, .map, .sum_type] {
		if has_custom_str {
			return '${fn_name}(it.${c_name(field_name)})'
		}
		return 'indent_${fn_name}(it.${c_name(field_name)}, indent_count + 1)'
	} else if sym.kind == .function {
		return '${fn_name}()'
	} else {
		if sym.kind == .chan {
			return '${fn_name}(it.${c_name(field_name)})'
		}
		mut method_str := 'it.${c_name(field_name)}'
		if sym.kind == .bool {
			method_str += ' ? _SLIT("true") : _SLIT("false")'
		} else if (field_type.is_int_valptr() || field_type.is_float_valptr())
			&& field_type.is_ptr() && !expects_ptr {
			// ptr int can be "nil", so this needs to be casted to a string
			if sym.kind == .f32 {
				return 'str_intp(1, _MOV((StrIntpData[]){
					{_SLIT0, $si_g32_code, {.d_f32 = *$method_str }}
				}))'
			} else if sym.kind == .f64 {
				return 'str_intp(1, _MOV((StrIntpData[]){
					{_SLIT0, $si_g64_code, {.d_f64 = *$method_str }}
				}))'
			} else if sym.kind == .u64 {
				fmt_type := StrIntpType.si_u64
				return 'str_intp(1, _MOV((StrIntpData[]){{_SLIT0, ${u32(fmt_type) | 0xfe00}, {.d_u64 = *$method_str }}}))'
			}
			fmt_type := StrIntpType.si_i32
			return 'str_intp(1, _MOV((StrIntpData[]){{_SLIT0, ${u32(fmt_type) | 0xfe00}, {.d_i32 = *$method_str }}}))'
		}
		return method_str
	}
}
