// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module c

import v.ast
import v.util

fn should_use_indent_func(kind ast.Kind) bool {
	return kind in [.struct_, .alias, .array, .array_fixed, .map, .sum_type, .interface_]
}

fn (mut g Gen) gen_str_default(sym ast.TypeSymbol, styp string, str_fn_name string) {
	mut convertor := ''
	mut typename_ := ''
	if sym.parent_idx in ast.integer_type_idxs {
		convertor = 'int'
		typename_ = 'int'
	} else if sym.parent_idx == ast.f32_type_idx {
		convertor = 'float'
		typename_ = 'f32'
	} else if sym.parent_idx == ast.f64_type_idx {
		convertor = 'double'
		typename_ = 'f64'
	} else if sym.parent_idx == ast.bool_type_idx {
		convertor = 'bool'
		typename_ = 'bool'
	} else {
		verror("could not generate string method for type '$styp'")
	}
	g.type_definitions.writeln('string ${str_fn_name}($styp it); // auto')
	g.auto_str_funcs.writeln('string ${str_fn_name}($styp it) {')
	if convertor == 'bool' {
		g.auto_str_funcs.writeln('\tstring tmp1 = string_add(_SLIT("${styp}("), ($convertor)it ? _SLIT("true") : _SLIT("false"));')
	} else {
		g.auto_str_funcs.writeln('\tstring tmp1 = string_add(_SLIT("${styp}("), tos3(${typename_}_str(($convertor)it).str));')
	}
	g.auto_str_funcs.writeln('\tstring tmp2 = string_add(tmp1, _SLIT(")"));')
	g.auto_str_funcs.writeln('\tstring_free(&tmp1);')
	g.auto_str_funcs.writeln('\treturn tmp2;')
	g.auto_str_funcs.writeln('}')
}

fn (g &Gen) type_to_fmt(typ ast.Type) string {
	if typ == ast.byte_type_idx {
		return '%hhx\\000'
	}
	if typ == ast.char_type_idx {
		return '%c\\000'
	}
	if typ == ast.voidptr_type_idx || typ in ast.byteptr_types {
		return '%p\\000'
	}
	if typ in ast.charptr_types {
		return '%C\\000' // a C string
	}
	sym := g.table.get_type_symbol(typ)
	if typ.is_ptr() && (typ.is_int_valptr() || typ.is_float_valptr()) {
		return '%.*s\\000'
	} else if sym.kind in [.struct_, .array, .array_fixed, .map, .bool, .enum_, .interface_,
		.sum_type, .function, .alias] {
		return '%.*s\\000'
	} else if sym.kind == .string {
		return "'%.*s\\000'"
	} else if sym.kind in [.f32, .f64] {
		return '%g\\000' // g removes trailing zeros unlike %f
	} else if sym.kind == .int {
		return '%d\\000'
	} else if sym.kind == .u32 {
		return '%u\\000'
	} else if sym.kind == .u64 {
		return '%llu\\000'
	} else if sym.kind == .i64 {
		return '%lld\\000'
	}
	return '%d\\000'
}

fn (mut g Gen) gen_str_for_type(typ ast.Type) string {
	styp := g.typ(typ).replace('*', '')
	mut sym := g.table.get_type_symbol(g.unwrap_generic(typ))
	mut str_fn_name := styp_to_str_fn_name(styp)
	if mut sym.info is ast.Alias {
		if sym.info.is_import {
			sym = g.table.get_type_symbol(sym.info.parent_type)
			str_fn_name = styp_to_str_fn_name(sym.name)
		}
	}
	sym_has_str_method, str_method_expects_ptr, str_nr_args := sym.str_method_info()
	already_generated_key := '$styp:$str_fn_name'
	if !sym_has_str_method && already_generated_key !in g.str_types && !typ.has_flag(.optional) {
		$if debugautostr ? {
			eprintln('> gen_str_for_type: |typ: ${typ:5}, ${sym.name:20}|has_str: ${sym_has_str_method:5}|expects_ptr: ${str_method_expects_ptr:5}|nr_args: ${str_nr_args:1}|fn_name: ${str_fn_name:20}')
		}
		g.str_types << already_generated_key
		match mut sym.info {
			ast.Alias {
				if sym.info.is_import {
					g.gen_str_default(sym, styp, str_fn_name)
				} else {
					g.gen_str_for_alias(sym.info, styp, str_fn_name)
				}
			}
			ast.Array {
				g.gen_str_for_array(sym.info, styp, str_fn_name)
			}
			ast.ArrayFixed {
				g.gen_str_for_array_fixed(sym.info, styp, str_fn_name)
			}
			ast.Enum {
				g.gen_str_for_enum(sym.info, styp, str_fn_name)
			}
			ast.FnType {
				g.gen_str_for_fn_type(sym.info, styp, str_fn_name)
			}
			ast.Struct {
				g.gen_str_for_struct(sym.info, styp, str_fn_name)
			}
			ast.Map {
				g.gen_str_for_map(sym.info, styp, str_fn_name)
			}
			ast.MultiReturn {
				g.gen_str_for_multi_return(sym.info, styp, str_fn_name)
			}
			ast.SumType {
				g.gen_str_for_union_sum_type(sym.info, styp, str_fn_name)
			}
			ast.Interface {
				g.gen_str_for_interface(sym.info, styp, str_fn_name)
			}
			else {
				verror("could not generate string method $str_fn_name for type '$styp'")
			}
		}
	}
	if typ.has_flag(.optional) {
		option_already_generated_key := 'option_$already_generated_key'
		if option_already_generated_key !in g.str_types {
			g.gen_str_for_option(typ, styp, str_fn_name)
			g.str_types << option_already_generated_key
		}
		return str_fn_name
	}
	return str_fn_name
}

fn (mut g Gen) gen_str_for_option(typ ast.Type, styp string, str_fn_name string) {
	parent_type := typ.clear_flag(.optional)
	sym := g.table.get_type_symbol(parent_type)
	sym_has_str_method, _, _ := sym.str_method_info()
	mut parent_str_fn_name := styp_to_str_fn_name(sym.cname)
	if !sym_has_str_method {
		parent_str_fn_name = g.gen_str_for_type(parent_type)
	}
	g.type_definitions.writeln('string ${str_fn_name}($styp it); // auto')
	g.auto_str_funcs.writeln('string ${str_fn_name}($styp it) { return indent_${str_fn_name}(it, 0); }')
	g.type_definitions.writeln('string indent_${str_fn_name}($styp it, int indent_count); // auto')
	g.auto_str_funcs.writeln('string indent_${str_fn_name}($styp it, int indent_count) {')
	g.auto_str_funcs.writeln('\tstring res;')
	g.auto_str_funcs.writeln('\tif (it.state == 0) {')
	if sym.kind == .string {
		g.auto_str_funcs.writeln('\t\tres = _STR("\'%.*s\\000\'", 2, ${parent_str_fn_name}(*($sym.cname*)it.data));')
	} else if should_use_indent_func(sym.kind) && !sym_has_str_method {
		g.auto_str_funcs.writeln('\t\tres = indent_${parent_str_fn_name}(*($sym.cname*)it.data, indent_count);')
	} else {
		g.auto_str_funcs.writeln('\t\tres = ${parent_str_fn_name}(*($sym.cname*)it.data);')
	}
	g.auto_str_funcs.writeln('\t} else {')
	g.auto_str_funcs.writeln('\t\tres = _STR("error: %.*s\\000", 2, IError_str(it.err));')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('\treturn _STR("Option(%.*s\\000)", 2, res);')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_alias(info ast.Alias, styp string, str_fn_name string) {
	sym := g.table.get_type_symbol(info.parent_type)
	sym_has_str_method, _, _ := sym.str_method_info()
	mut parent_str_fn_name := styp_to_str_fn_name(sym.name.replace('.', '__'))
	if !sym_has_str_method {
		parent_str_fn_name = g.gen_str_for_type(info.parent_type)
	}
	mut clean_type_v_type_name := util.strip_main_name(styp.replace('__', '.'))
	g.type_definitions.writeln('static string ${str_fn_name}($styp it); // auto')
	g.auto_str_funcs.writeln('static string ${str_fn_name}($styp it) { return indent_${str_fn_name}(it, 0); }')
	g.type_definitions.writeln('static string indent_${str_fn_name}($styp it, int indent_count); // auto')
	g.auto_str_funcs.writeln('static string indent_${str_fn_name}($styp it, int indent_count) {')
	g.auto_str_funcs.writeln('\tstring indents = _SLIT("");')
	g.auto_str_funcs.writeln('\tfor (int i = 0; i < indent_count; ++i) {')
	g.auto_str_funcs.writeln('\t\tindents = string_add(indents, _SLIT("    "));')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('\treturn _STR("%.*s\\000${clean_type_v_type_name}(%.*s\\000)", 3, indents, ${parent_str_fn_name}(it));')
	g.auto_str_funcs.writeln('}')
}

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
			g.auto_str_funcs.writeln('\t\tstring x = _STR("%g", 1, it);')
		} else if sym.kind == .rune {
			g.auto_str_funcs.writeln('\t\tstring x = _STR("`%.*s\\000`", 2, ${elem_str_fn_name}(it));')
		} else if sym.kind == .string {
			g.auto_str_funcs.writeln('\t\tstring x = _STR("\'%.*s\\000\'", 2, it);')
		} else {
			// There is a custom .str() method, so use it.
			// NB: we need to take account of whether the user has defined
			// `fn (x T) str() {` or `fn (x &T) str() {`, and convert accordingly
			if (str_method_expects_ptr && is_elem_ptr) || (!str_method_expects_ptr && !is_elem_ptr) {
				g.auto_str_funcs.writeln('\t\tstring x = ${elem_str_fn_name}(it);')
			} else if str_method_expects_ptr && !is_elem_ptr {
				g.auto_str_funcs.writeln('\t\tstring x = ${elem_str_fn_name}(&it);')
			} else if !str_method_expects_ptr && is_elem_ptr {
				g.auto_str_funcs.writeln('\t\tstring x = ${elem_str_fn_name}(*it);')
			}
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
		g.auto_str_funcs.writeln('\tfor (int i = 0; i < $info.size; ++i) {')
		if should_use_indent_func(sym.kind) && !sym_has_str_method {
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${elem_str_fn_name}(a[i]));')
		} else if sym.kind in [.f32, .f64] {
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _STR("%g", 1, a[i]));')
		} else if sym.kind == .string {
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _STR("\'%.*s\\000\'", 2, a[i]));')
		} else if sym.kind == .rune {
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _STR("`%.*s\\000`", 2, ${elem_str_fn_name}(a[i])));')
		} else {
			if (str_method_expects_ptr && is_elem_ptr) || (!str_method_expects_ptr && !is_elem_ptr) {
				g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${elem_str_fn_name}(a[i]));')
			} else if str_method_expects_ptr && !is_elem_ptr {
				g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${elem_str_fn_name}(&a[i]));')
			} else if !str_method_expects_ptr && is_elem_ptr {
				g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${elem_str_fn_name}(*a[i]));')
			}
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
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _STR("\'%.*s\\000\'", 2, key));')
	} else if key_sym.kind == .rune {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _STR("`%.*s\\000`", 2, ${key_str_fn_name}(key)));')
	} else {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${key_str_fn_name}(key));')
	}
	g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _SLIT(": "));')
	if val_sym.kind == .function {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${elem_str_fn_name}());')
	} else if val_sym.kind == .string {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _STR("\'%.*s\\000\'", 2, *($val_styp*)DenseArray_value(&m.key_values, i)));')
	} else if should_use_indent_func(val_sym.kind) && !val_sym.has_method('str') {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, indent_${elem_str_fn_name}(*($val_styp*)DenseArray_value(&m.key_values, i), indent_count));')
	} else if val_sym.kind in [.f32, .f64] {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _STR("%g", 1, *($val_styp*)DenseArray_value(&m.key_values, i)));')
	} else if val_sym.kind == .rune {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _STR("`%.*s\\000`", 2, ${elem_str_fn_name}(*($val_styp*)DenseArray_value(&m.key_values, i))));')
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

fn (mut g Gen) gen_str_for_multi_return(info ast.MultiReturn, styp string, str_fn_name string) {
	for typ in info.types {
		sym := g.table.get_type_symbol(typ)
		if !sym.has_method('str') {
			g.gen_str_for_type(typ)
		}
	}
	g.type_definitions.writeln('static string ${str_fn_name}($styp a); // auto')
	g.auto_str_funcs.writeln('static string ${str_fn_name}($styp a) {')
	g.auto_str_funcs.writeln('\tstrings__Builder sb = strings__new_builder($info.types.len * 10);')
	g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, _SLIT("("));')
	for i, typ in info.types {
		sym := g.table.get_type_symbol(typ)
		field_styp := g.typ(typ)
		is_arg_ptr := typ.is_ptr()
		sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
		mut arg_str_fn_name := ''
		if sym_has_str_method {
			arg_str_fn_name = if is_arg_ptr {
				field_styp.replace('*', '') + '_str'
			} else {
				field_styp + '_str'
			}
		} else {
			arg_str_fn_name = styp_to_str_fn_name(field_styp)
		}
		if should_use_indent_func(sym.kind) && !sym_has_str_method {
			g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, ${arg_str_fn_name}(a.arg$i));')
		} else if sym.kind in [.f32, .f64] {
			g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, _STR("%g", 1, a.arg$i));')
		} else if sym.kind == .string {
			g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, _STR("\'%.*s\\000\'", 2, a.arg$i));')
		} else if sym.kind == .function {
			g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, ${arg_str_fn_name}());')
		} else {
			if (str_method_expects_ptr && is_arg_ptr) || (!str_method_expects_ptr && !is_arg_ptr) {
				g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, ${arg_str_fn_name}(a.arg$i));')
			} else if str_method_expects_ptr && !is_arg_ptr {
				g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, ${arg_str_fn_name}(&a.arg$i));')
			} else if !str_method_expects_ptr && is_arg_ptr {
				g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, ${arg_str_fn_name}(*a.arg$i));')
			}
		}
		if i != info.types.len - 1 {
			g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, _SLIT(", "));')
		}
	}
	g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, _SLIT(")"));')
	g.auto_str_funcs.writeln('\tstring res = strings__Builder_str(&sb);')
	g.auto_str_funcs.writeln('\tstrings__Builder_free(&sb);')
	g.auto_str_funcs.writeln('\treturn res;')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_struct(info ast.Struct, styp string, str_fn_name string) {
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
			clean_struct_v_type_name.replace('_Array', '_array').replace('_T_', '<').replace('_', ', ') +
			'>'
	}
	clean_struct_v_type_name = util.strip_main_name(clean_struct_v_type_name)
	// generate ident / indent length = 4 spaces
	g.auto_str_funcs.writeln('\tstring indents = _SLIT("");')
	g.auto_str_funcs.writeln('\tfor (int i = 0; i < indent_count; ++i) {')
	g.auto_str_funcs.writeln('\t\tindents = string_add(indents, _SLIT("    "));')
	g.auto_str_funcs.writeln('\t}')
	if info.fields.len == 0 {
		g.auto_str_funcs.write_string('\treturn _SLIT("$clean_struct_v_type_name{}");')
	} else {
		g.auto_str_funcs.write_string('\treturn _STR("$clean_struct_v_type_name{\\n"')
		for field in info.fields {
			mut fmt := if field.typ.is_ptr() { '&' } else { '' }
			fmt += g.type_to_fmt(field.typ)
			g.auto_str_funcs.writeln('\t\t"%.*s\\000    $field.name: $fmt\\n"')
		}
		g.auto_str_funcs.write_string('\t\t"%.*s\\000}", ${2 * (info.fields.len + 1)}')
		if info.fields.len > 0 {
			g.auto_str_funcs.write_string(',\n\t\t')
			for i, field in info.fields {
				sym := g.table.get_type_symbol(field.typ)
				has_custom_str := sym.has_method('str')
				mut field_styp := g.typ(field.typ).replace('*', '')
				field_styp_fn_name := if has_custom_str {
					'${field_styp}_str'
				} else {
					fnames2strfunc[field_styp]
				}
				g.auto_str_funcs.write_string('indents, ')
				mut func := struct_auto_str_func(sym, field.typ, field_styp_fn_name, field.name)
				// reference types can be "nil"
				if field.typ.is_ptr() && !(field.typ in ast.charptr_types
					|| field.typ in ast.byteptr_types
					|| field.typ == ast.voidptr_type_idx) {
					g.auto_str_funcs.write_string('isnil(it.${c_name(field.name)})')
					g.auto_str_funcs.write_string(' ? _SLIT("nil") : ')
					// struct, floats and ints have a special case through the _str function
					if sym.kind != .struct_ && !field.typ.is_int_valptr()
						&& !field.typ.is_float_valptr() {
						g.auto_str_funcs.write_string('*')
					}
				}
				// handle circular ref type of struct to the struct itself
				if styp == field_styp {
					g.auto_str_funcs.write_string('_SLIT("<circular>")')
				} else {
					g.auto_str_funcs.write_string(func)
				}

				if i < info.fields.len - 1 {
					g.auto_str_funcs.write_string(',\n\t\t')
				}
			}
		}
		g.auto_str_funcs.writeln(',')
		g.auto_str_funcs.writeln('\t\tindents);')
	}
	g.auto_str_funcs.writeln('}')
}

fn struct_auto_str_func(sym &ast.TypeSymbol, field_type ast.Type, fn_name string, field_name string) string {
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
		mut method_str := 'it.${c_name(field_name)}'
		if sym.kind == .bool {
			method_str += ' ? _SLIT("true") : _SLIT("false")'
		} else if (field_type.is_int_valptr() || field_type.is_float_valptr())
			&& field_type.is_ptr() && !expects_ptr {
			// ptr int can be "nil", so this needs to be castet to a string
			fmt := if sym.kind in [.f32, .f64] {
				'%g\\000'
			} else if sym.kind == .u64 {
				'%lld\\000'
			} else {
				'%d\\000'
			}
			method_str = '_STR("$fmt", 2, *$method_str)'
		}
		return method_str
	}
}

fn (mut g Gen) gen_str_for_enum(info ast.Enum, styp string, str_fn_name string) {
	s := util.no_dots(styp)
	g.type_definitions.writeln('static string ${str_fn_name}($styp it); // auto')
	g.auto_str_funcs.writeln('static string ${str_fn_name}($styp it) { /* gen_str_for_enum */')
	// Enums tagged with `[flag]` are special in that they can be a combination of enum values
	if info.is_flag {
		clean_name := util.strip_main_name(styp.replace('__', '.'))
		g.auto_str_funcs.writeln('\tstring ret = _SLIT("$clean_name{");')
		g.auto_str_funcs.writeln('\tint first = 1;')
		for i, val in info.vals {
			g.auto_str_funcs.writeln('\tif (it & (1 << $i)) {if (!first) {ret = string_add(ret, _SLIT(" | "));} ret = string_add(ret, _SLIT(".$val")); first = 0;}')
		}
		g.auto_str_funcs.writeln('\tret = string_add(ret, _SLIT("}"));')
		g.auto_str_funcs.writeln('\treturn ret;')
	} else {
		g.auto_str_funcs.writeln('\tswitch(it) {')
		// Only use the first multi value on the lookup
		mut seen := []string{len: info.vals.len}
		for val in info.vals {
			if info.is_multi_allowed && val in seen {
				continue
			} else if info.is_multi_allowed {
				seen << val
			}
			g.auto_str_funcs.writeln('\t\tcase ${s}_$val: return _SLIT("$val");')
		}
		g.auto_str_funcs.writeln('\t\tdefault: return _SLIT("unknown enum value");')
		g.auto_str_funcs.writeln('\t}')
	}
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_interface(info ast.Interface, styp string, str_fn_name string) {
	mut gen_fn_names := map[string]string{}
	for typ in info.types {
		sym := g.table.get_type_symbol(typ)
		if !sym.has_method('str') {
			field_styp := g.typ(typ)
			field_fn_name := g.gen_str_for_type(typ)
			gen_fn_names[field_styp] = field_fn_name
		}
	}
	mut clean_interface_v_type_name := styp.replace('__', '.')
	if styp.ends_with('*') {
		clean_interface_v_type_name = '&' + clean_interface_v_type_name.replace('*', '')
	}
	clean_interface_v_type_name = util.strip_main_name(clean_interface_v_type_name)

	g.type_definitions.writeln('static string ${str_fn_name}($styp x); // auto')
	g.auto_str_funcs.writeln('static string ${str_fn_name}($styp x) { return indent_${str_fn_name}(x, 0); }')
	g.type_definitions.writeln('static string indent_${str_fn_name}($styp x, int indent_count); // auto')
	g.auto_str_funcs.writeln('static string indent_${str_fn_name}($styp x, int indent_count) { /* gen_str_for_interface */')
	for typ in info.types {
		typ_str := g.typ(typ)
		subtype := g.table.get_type_symbol(typ)

		mut func_name := if typ_str in gen_fn_names {
			gen_fn_names[typ_str]
		} else {
			g.gen_str_for_type(typ)
		}

		sym_has_str_method, str_method_expects_ptr, _ := subtype.str_method_info()
		if should_use_indent_func(subtype.kind) && !sym_has_str_method {
			func_name = 'indent_$func_name'
		}
		deref := if sym_has_str_method && str_method_expects_ptr { ' ' } else { '*' }
		value_fmt := if typ == ast.string_type { "'%.*s\\000'" } else { '%.*s\\000' }

		g.auto_str_funcs.write_string('\tif (x._typ == _${styp}_${subtype.cname}_index)')
		g.auto_str_funcs.write_string(' return _STR("${clean_interface_v_type_name}($value_fmt)", 2, ')
		g.auto_str_funcs.write_string('${func_name}(${deref}($subtype.cname*)x._$subtype.cname')
		if should_use_indent_func(subtype.kind) && !sym_has_str_method {
			g.auto_str_funcs.write_string(', indent_count')
		}
		g.auto_str_funcs.writeln('));')
	}
	g.auto_str_funcs.writeln('\treturn _SLIT("unknown interface value");')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_union_sum_type(info ast.SumType, styp string, str_fn_name string) {
	mut gen_fn_names := map[string]string{}
	for typ in info.variants {
		sym := g.table.get_type_symbol(typ)
		if !sym.has_method('str') {
			field_styp := g.typ(typ)
			field_fn_name := g.gen_str_for_type(typ)
			gen_fn_names[field_styp] = field_fn_name
		}
	}
	// _str() functions should have a single argument, the indenting ones take 2:
	g.type_definitions.writeln('static string ${str_fn_name}($styp x); // auto')
	g.auto_str_funcs.writeln('static string ${str_fn_name}($styp x) { return indent_${str_fn_name}(x, 0); }')
	g.type_definitions.writeln('static string indent_${str_fn_name}($styp x, int indent_count); // auto')
	g.auto_str_funcs.writeln('static string indent_${str_fn_name}($styp x, int indent_count) {')
	mut clean_sum_type_v_type_name := styp.replace('__', '.')
	if styp.ends_with('*') {
		clean_sum_type_v_type_name = '&' + clean_sum_type_v_type_name.replace('*', '')
	}
	clean_sum_type_v_type_name = util.strip_main_name(clean_sum_type_v_type_name)
	g.auto_str_funcs.writeln('\tswitch(x._typ) {')
	for typ in info.variants {
		mut value_fmt := '%.*s\\000'
		if typ == ast.string_type {
			value_fmt = "'$value_fmt'"
		}
		typ_str := g.typ(typ)
		mut func_name := if typ_str in gen_fn_names {
			gen_fn_names[typ_str]
		} else {
			g.gen_str_for_type(typ)
		}
		sym := g.table.get_type_symbol(typ)
		sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
		deref := if sym_has_str_method && str_method_expects_ptr { ' ' } else { '*' }
		if should_use_indent_func(sym.kind) && !sym_has_str_method {
			func_name = 'indent_$func_name'
		}
		g.auto_str_funcs.write_string('\t\tcase $typ: return _STR("${clean_sum_type_v_type_name}($value_fmt)", 2, ${func_name}(${deref}($typ_str*)x._$sym.cname')
		if should_use_indent_func(sym.kind) && !sym_has_str_method {
			g.auto_str_funcs.write_string(', indent_count')
		}
		g.auto_str_funcs.writeln('));')
	}
	g.auto_str_funcs.writeln('\t\tdefault: return _SLIT("unknown sum type value");')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) fn_decl_str(info ast.FnType) string {
	mut fn_str := 'fn ('
	for i, arg in info.func.params {
		if i > 0 {
			fn_str += ', '
		}
		fn_str += util.strip_main_name(g.table.get_type_name(arg.typ))
	}
	fn_str += ')'
	if info.func.return_type != ast.void_type {
		fn_str += ' ${util.strip_main_name(g.table.get_type_name(info.func.return_type))}'
	}
	return fn_str
}

fn (mut g Gen) gen_str_for_fn_type(info ast.FnType, styp string, str_fn_name string) {
	g.type_definitions.writeln('static string ${str_fn_name}(); // auto')
	g.auto_str_funcs.writeln('static string ${str_fn_name}() { return _SLIT("${g.fn_decl_str(info)}");}')
}

[inline]
fn styp_to_str_fn_name(styp string) string {
	return styp.replace_each(['*', '', '.', '__', ' ', '__']) + '_str'
}
